package com.whenly.service;

import com.ericsson.otp.erlang.*;
import com.whenly.model.Constraint;
import com.whenly.model.Event;
import com.whenly.repository.ConstraintRepository;
import com.whenly.repository.EventRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.*;

@Service
public class ConstraintService {

    // Repository for accessing constraint data in the database
    @Autowired
    private ConstraintRepository constraintRepository;

    // Repository for accessing event data in the database
    @Autowired
    private EventRepository eventRepository;

    // Shared list to manage the available Erlang node addresses
    @Autowired
    private SharedStringList sharedStringList;

    // API to communicate with the Erlang backend nodes
    @Autowired
    private ErlangBackendAPI erlangBackendAPI;

    // Formatter to parse date strings in the format "yyyy-MM-dd HH:mm"
    private final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");

    // Index used for round-robin selection of Erlang nodes
    private int currentNodeIndex = 0;

    /**
     * Adds constraints to a given event.
     *
     * @param eventId         The ID of the event.
     * @param constraintsList A list of constraint strings in the format "lowerLimit,upperLimit".
     * @param username        The user who is adding the constraints.
     * @return ResponseEntity with a confirmation message and the assigned Erlang node.
     */
    public ResponseEntity<Map<String, String>> addConstraint(Long eventId, List<String> constraintsList, String username) {
        // Check if the event exists in the repository
        Optional<Event> eventOpt = eventRepository.findById(eventId);
        if (eventOpt.isEmpty()) {
            throw new ResponseStatusException(HttpStatus.NOT_FOUND, "Event not found");
        }
        
        Event event = eventOpt.get();
        
        // Parse and validate each constraint from the provided list,
        // converting the date strings into Unix timestamps (in seconds)
        List<Map<String, String>> parsedConstraints = new ArrayList<>();
        for (String constraint : constraintsList) {
            try {
                // Split the string into two parts: lowerLimit and upperLimit
                String[] limits = constraint.split(",");
                if (limits.length != 2) {
                    throw new IllegalArgumentException("Invalid constraint format. Expected 'lowerLimit,upperLimit'.");
                }
                
                // Parse the date strings using the defined formatter
                LocalDateTime lowerLimit = LocalDateTime.parse(limits[0].trim(), formatter);
                LocalDateTime upperLimit = LocalDateTime.parse(limits[1].trim(), formatter);
                
                // Validate that the lower limit is before the upper limit
                if (lowerLimit.isAfter(upperLimit)) {
                    throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Lower limit must be before upper limit.");
                }
                
                // Convert the dates to Unix timestamps (using an offset of +1 hour)
                long lowerEpoch = lowerLimit.toEpochSecond(ZoneOffset.ofHours(1));
                long upperEpoch = upperLimit.toEpochSecond(ZoneOffset.ofHours(1));
          
                // Add the parsed constraint as a map with string representations of the timestamps
                parsedConstraints.add(Map.of(
                    "lowerLimit", String.valueOf(lowerEpoch),
                    "upperLimit", String.valueOf(upperEpoch)
                ));
            } catch (Exception e) {
                // If any parsing error occurs, return a 400 BAD REQUEST response
                throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Invalid date format. Use 'YYYY-MM-DD HH:mm'");
            }
        }
        
        // Select an available Erlang node using round-robin selection
        String assignedNode = selectErlangNode();
        if (assignedNode == null) {
            throw new ResponseStatusException(HttpStatus.SERVICE_UNAVAILABLE, "No available Erlang nodes");
        }
        
        // Send the constraints to the selected Erlang node
        boolean sentSuccessfully = sendConstraintsToErlang(eventId, parsedConstraints, assignedNode);
        
        if (!sentSuccessfully) {
            throw new ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR, "Failed to communicate with Erlang node");
        }
        
        // Save the new constraint record in the database (stored as JSON)
        try {
            Constraint newConstraint = new Constraint(event, username, assignedNode, parsedConstraints);
            constraintRepository.save(newConstraint);
        } catch (Exception e) {
            throw new ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR, "Failed to save constraints to database");
        }
        
        // Prepare and return the HTTP response with a success message and the node that handled the request
        Map<String, String> response = new HashMap<>();
        response.put("message", "Constraints added successfully");
        response.put("assignedNode", assignedNode);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }
    
    /**
     * Selects an available Erlang node using a Round Robin algorithm.
     * Access to the node list is thread-safe.
     *
     * @return The IP address of the selected node or null if the list is empty.
     */
    private String selectErlangNode() {
        // Retrieve the list of available Erlang nodes
        List<String> nodes = sharedStringList.getStrings();
        if (nodes.isEmpty()) {
            return null; // No available nodes
        }

        // Use round-robin selection to choose the next node
        synchronized (this) {
            String selectedNode = nodes.get(currentNodeIndex);
            currentNodeIndex = (currentNodeIndex + 1) % nodes.size();
            return selectedNode;
        }
    }

    /**
     * Sends the parsed constraints to the Erlang cluster via the ErlangBackendAPI.
     *
     * @param eventId         The ID of the event.
     * @param constraintsList The list of parsed constraints (with Unix timestamps).
     * @param assignedNode    The selected Erlang node.
     * @return True if the constraints were sent successfully, false otherwise.
     */
    public boolean sendConstraintsToErlang(Long eventId, List<Map<String, String>> constraintsList, String assignedNode) {
        //System.out.println("These are the constraintsList: " + constraintsList);
        
        try {
            // Construct an Erlang list of constraint tuples,
            // converting the string timestamp values into OtpErlangLong objects
            OtpErlangList erlangConstraints = new OtpErlangList(
                constraintsList.stream()
                    .map(constraint -> {
                        // Retrieve the Unix timestamps for the lower and upper limits
                        long lowerLimit = Long.parseLong(constraint.get("lowerLimit"));
                        long upperLimit = Long.parseLong(constraint.get("upperLimit"));

                        // Create an Erlang tuple {lowerLimit, upperLimit}
                        return new OtpErlangTuple(new OtpErlangObject[]{
                            new OtpErlangLong(lowerLimit),
                            new OtpErlangLong(upperLimit)
                        });
                    })
                    .toArray(OtpErlangObject[]::new)
            );
            
            // Send the message containing the constraints to the specified Erlang node
            erlangBackendAPI.addConstraint(assignedNode, String.valueOf(eventId), erlangConstraints);
            return true;
        } catch (Exception e) {
            // Print the stack trace for debugging and indicate failure
            e.printStackTrace();
            return false;
        }
    }
    
}
