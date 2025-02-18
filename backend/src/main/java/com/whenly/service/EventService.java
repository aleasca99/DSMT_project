package com.whenly.service;

import com.whenly.events.FinalSolutionEvent;
import com.whenly.events.NodeStatusEvent;
import com.whenly.model.Constraint;
import com.whenly.model.Event;
import com.whenly.model.User;
import com.whenly.repository.ConstraintRepository;
import com.whenly.repository.EventRepository;
import com.whenly.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;
import com.ericsson.otp.erlang.*;

import java.math.BigInteger;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.stream.Collectors;

@Service
public class EventService {

    // Repository for accessing Event entities in the database
    @Autowired
    private EventRepository eventRepository;

    // Repository for accessing Constraint entities in the database
    @Autowired
    private ConstraintRepository constraintRepository;

    // Repository for accessing User entities in the database
    @Autowired
    private UserRepository userRepository;

    // ConstraintService is injected to handle sending constraints to Erlang nodes
    @Autowired
    private ConstraintService constraintService;

    // ErlangBackendAPI to send messages to the Erlang backend
    @Autowired
    private ErlangBackendAPI erlangBackendAPI;

    // Shared list containing active Erlang node IP addresses
    @Autowired
    private SharedStringList sharedStringList;

    // Formatter for parsing deadline date strings in "yyyy-MM-dd HH:mm" format
    private final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");

    // Index used for round-robin selection of Erlang nodes
    private int currentNodeIndex = 0;

    /**
     * Creates a new event with the provided event name, deadline, and creator username.
     *
     * @param eventName the name of the event
     * @param deadline  the event deadline in the format "yyyy-MM-dd HH:mm"
     * @param username  the username of the creator
     * @return a ResponseEntity containing a confirmation message, event ID, and the assigned Erlang node
     */
    public ResponseEntity<Map<String, String>> createEvent(String eventName, String deadline, String username) {
        // Find the user by username
        Optional<User> userOpt = userRepository.findByUsername(username);
        if (userOpt.isEmpty()) {
            throw new ResponseStatusException(HttpStatus.NOT_FOUND, "User not found");
        }
        
        // Parse the deadline string into a LocalDateTime object
        LocalDateTime parsedDeadline;
        try {
            parsedDeadline = LocalDateTime.parse(deadline, formatter);
        } catch (Exception e) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Invalid deadline format. Use 'YYYY-MM-DD HH:mm'");
        }
        
        // Select an available Erlang node using a round-robin strategy
        String assignedErlangNode = selectErlangNode();
        if (assignedErlangNode == null) {
            throw new ResponseStatusException(HttpStatus.SERVICE_UNAVAILABLE, "No available Erlang nodes");
        }
        
        // Create and save the new event
        Event newEvent = new Event(eventName, userOpt.get().getUsername(), parsedDeadline, assignedErlangNode);
        eventRepository.save(newEvent);
        Long eventId = newEvent.getId();
        System.out.println("Event created successfully with ID: " + eventId);
        System.out.println("Sending event to Erlang backend...");
        
        // Send the event details to the Erlang backend
        boolean success = sendEventToErlang(eventId, parsedDeadline, assignedErlangNode);
        if (!success) {
            throw new ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR, "Failed to notify Erlang backend");
        }
        
        // Prepare and return the response
        Map<String, String> response = new HashMap<>();
        response.put("message", "Event created successfully");
        response.put("eventId", String.valueOf(eventId));
        response.put("assignedErlangNode", assignedErlangNode);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    /**
     * Selects an available Erlang node using a round-robin algorithm.
     *
     * @return the selected Erlang node IP as a String, or null if no nodes are available.
     */
    private String selectErlangNode() {
        List<String> activeNodes = sharedStringList.getStrings();
        if (activeNodes.isEmpty()) {
            return null;
        }
        // Determine the index using the current round-robin index
        int index = currentNodeIndex % activeNodes.size();
        currentNodeIndex++;
        return activeNodes.get(index);
    }

    /**
     * Sends the event details to the Erlang backend.
     *
     * @param eventID            the event ID
     * @param deadline           the event deadline as a LocalDateTime
     * @param assignedErlangNode the Erlang node assigned to manage the event
     * @return true if the event was sent successfully, false otherwise
     */
    public boolean sendEventToErlang(Long eventID, LocalDateTime deadline, String assignedErlangNode) {
        try {
            // Convert the deadline to a Unix timestamp (using an offset of +1 hour)
            long unixDeadline = deadline.toEpochSecond(ZoneOffset.ofHours(1));
            // Create an empty Erlang list for constraints (if any)
            OtpErlangList constraints = new OtpErlangList();
            System.out.println("Sending event to Erlang backend: " + eventID.toString());
            // Call the createEvent method in ErlangBackendAPI to send the event details
            erlangBackendAPI.createEvent(assignedErlangNode, eventID.toString(), unixDeadline, constraints);
            return true;
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }

    /**
     * Adds an Erlang node's IP address to the list of active nodes.
     *
     * @param nodeIp the IP address of the Erlang node to add
     */
    public void addErlangNode(String nodeIp) {
        sharedStringList.addString(nodeIp);
    }

    /**
     * Removes an Erlang node's IP address from the list of active nodes.
     *
     * @param nodeIp the IP address of the Erlang node to remove
     */
    public void removeErlangNode(String nodeIp) {
        sharedStringList.removeString(nodeIp);
    }

    /**
     * Updates the final solution for an event based on the solution string received from Erlang.
     *
     * @param eventId       the event ID
     * @param finalSolution the final solution string (expected to be in tuple format)
     */
    public void updateFinalSolution(Long eventId, String finalSolution) {
        // Retrieve the event from the repository
        Optional<Event> eventOpt = eventRepository.findById(eventId);
        if (eventOpt.isEmpty()) {
            throw new ResponseStatusException(HttpStatus.NOT_FOUND, "Event not found");
        }
        
        // Clean up the final solution string by trimming and removing surrounding brackets
        String trimmedSolution = finalSolution.trim();
        if (trimmedSolution.startsWith("[{") && trimmedSolution.endsWith("}]")) {
            trimmedSolution = trimmedSolution.substring(1, trimmedSolution.length() - 1);
        }
        
        // Split the solution into its components
        String[] parts = trimmedSolution.split(",");
        if (parts.length < 2) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Invalid finalSolution tuple format. Expected two elements.");
        }
        // Remove extraneous characters from each element
        String firstElement = parts[0].trim().replaceAll("[\\[\\{\\]\\}]", "");
        String secondElement = parts[1].trim().replaceAll("[\\[\\{\\]\\}]", "");

        System.err.println("First element: " + firstElement);
        System.err.println("Second element: " + secondElement);

        // If the solution indicates 'undefined' or 'no_solution', update the event accordingly
        if (firstElement.equals("undefined") || firstElement.equals("no_solution")) {
            Event event = eventOpt.get();
            event.setFinalSolution(firstElement);
            eventRepository.save(event);
            return;
        }
        // Convert the timestamp values to LocalDateTime objects
        LocalDateTime firstElementDate = LocalDateTime.ofEpochSecond(Long.parseLong(firstElement), 0, ZoneOffset.ofHours(1));
        LocalDateTime secondElementDate = LocalDateTime.ofEpochSecond(Long.parseLong(secondElement), 0, ZoneOffset.ofHours(1));

        System.err.println("First element: " + firstElementDate);
        System.err.println("Second element: " + secondElementDate);

        // Create a string representation of the final solution using the parsed dates
        String solutionToInsert = firstElementDate.toString() + ", " + secondElementDate.toString();
        Event event = eventOpt.get();
        event.setFinalSolution(solutionToInsert);
        eventRepository.save(event);
    }

    /**
     * Retrieves a list of events to which the specified user has added constraints.
     *
     * @param username the username of the user
     * @return a ResponseEntity containing a list of event details as maps
     */
    public ResponseEntity<List<Map<String, String>>> getEventsByUser(String username) {
        // Retrieve event IDs (as BigInteger) from the constraints table for the given username
        List<BigInteger> rawIds = constraintRepository.findEventIdsByUsername(username);
        if (rawIds == null || rawIds.isEmpty()) {
            return ResponseEntity.ok(Collections.emptyList());
        }
        
        // Convert the raw IDs to a List<Long>
        List<Long> eventIds = rawIds.stream()
                                    .map(BigInteger::longValue)
                                    .collect(Collectors.toList());
        
        // Retrieve the corresponding events from the repository
        List<Event> events = eventRepository.findAllById(eventIds);
        
        // Map each event into a structure (Map<String, String>) to be returned in the response
        List<Map<String, String>> eventMaps = new ArrayList<>();
        for (Event event : events) {
            Map<String, String> map = new HashMap<>();
            map.put("eventId", String.valueOf(event.getId()));
            map.put("eventName", event.getEventName());
            map.put("creator", event.getCreatorUsername());
            map.put("deadline", event.getDeadline().toString());
            map.put("result", event.getFinalResult());
            map.put("erlangNodeIp", event.getErlangNodeIp());

            eventMaps.add(map);
        }
        
        return ResponseEntity.ok(eventMaps);
    }

    /**
     * Retrieves the details of a specific event by its ID.
     *
     * @param eventId the event ID
     * @return a ResponseEntity containing event details as a map
     */
    public ResponseEntity<Map<String, String>> getEventById(Long eventId) {
        Optional<Event> eventOpt = eventRepository.findById(eventId);
        if (eventOpt.isEmpty()) {
            throw new ResponseStatusException(HttpStatus.NOT_FOUND, "Event not found");
        }
        Event event = eventOpt.get();
        Map<String, String> response = new HashMap<>();
        response.put("eventId", String.valueOf(event.getId()));
        response.put("eventName", event.getEventName());
        response.put("creator", event.getCreatorUsername());
        response.put("deadline", event.getDeadline().toString());
        // response.put("erlangNodeIp", event.getErlangNodeIp());
        response.put("finalSolution", event.getFinalResult());
        return ResponseEntity.ok(response);
    }

    /**
     * Recovers events and constraints from a failed Erlang node.
     * This method reassigns constraints and events to active nodes.
     *
     * @param failedNode the IP address of the failed Erlang node
     */
    public void recoverFromNodeFailure(String failedNode) {
        // Retrieve constraints that were assigned to the failed node
        List<Constraint> constraintsToRecover = constraintRepository.findConstraintsByErlangNode(failedNode);
        List<String> activeNodes = sharedStringList.getStrings();
        if (activeNodes.isEmpty()) {
            throw new IllegalStateException("No active Erlang nodes available for recovery.");
        }

        // Filter constraints related only to events that do not yet have a final result
        constraintsToRecover = constraintsToRecover.stream()
                .filter(c -> c.getEvent().getFinalResult() == null)
                .collect(Collectors.toList());

        // Begin recovery for constraints
        System.out.println("Recovering from failed node: " + failedNode);
        for (Constraint constraint : constraintsToRecover) {
            // Select a new Erlang node using round-robin selection
            String newNode = selectErlangNode();
            if (newNode == null) {
                throw new IllegalStateException("No available Erlang nodes for redistribution.");
            }
            // Update the constraint with the new assigned node and save it
            constraint.setAssignedErlangNode(newNode);
            constraintRepository.save(constraint);
            // Use ConstraintService to send the constraints to the new node
            try {
                List<Map<String, String>> constraintsAsList = constraint.getConstraintsAsList();
                boolean sent = constraintService.sendConstraintsToErlang(
                        constraint.getEvent().getId(),
                        constraintsAsList,
                        newNode
                );
                if (!sent) {
                    // Handle the case where sending constraints fails
                    System.err.println("Failed to resend constraints for event " + constraint.getEvent().getId());
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        
        // Recovery for events assigned to the failed node
        List<Event> eventsToRecover = eventRepository.findByErlangNodeIpAndFinalResultIsNull(failedNode);
        for (Event event : eventsToRecover) {
            // Select a new manager node for the event using round-robin selection
            String newManagerNode = selectErlangNode();
            if (newManagerNode == null) {
                throw new IllegalStateException("No available Erlang nodes to assign as new manager.");
            }
            event.setErlangNodeIp(newManagerNode);
            eventRepository.save(event);
            // Notify the new Erlang node about the event creation
            erlangBackendAPI.createEvent(newManagerNode, event.getId().toString(),
                    event.getDeadline().toEpochSecond(ZoneOffset.ofHours(1)), new OtpErlangList());
        }
    }

    // --- EVENT LISTENERS FOR EVENTS PUBLISHED BY ErlangBackendAPI ---

    /**
     * Handles FinalSolutionEvent published by ErlangBackendAPI.
     *
     * @param event the FinalSolutionEvent containing the event ID and the solution
     */
    @EventListener
    public void handleFinalSolutionEvent(FinalSolutionEvent event) {
        System.out.println("EventService received FinalSolutionEvent for EventId: " 
                           + event.getEventId() + ", Solution: " + event.getSolution());
        // Update the event with the received final solution
        updateFinalSolution(Long.parseLong(event.getEventId()), event.getSolution());
    }

    /**
     * Handles NodeStatusEvent published by ErlangBackendAPI.
     *
     * @param event the NodeStatusEvent containing the node name and its status
     */
    @EventListener
    public void handleNodeStatusEvent(NodeStatusEvent event) {
        System.out.println("EventService received NodeStatusEvent for Node: " 
                           + event.getNodeName() + ", Status: " + event.getStatus());
        if ("up".equals(event.getStatus())) {
            // If the node is up, add it to the active nodes list and display active nodes
            addErlangNode(event.getNodeName());
            System.out.println("Active nodes: " + sharedStringList.getStrings());
        } else if ("down".equals(event.getStatus())) {
            // If the node is down, remove it and trigger recovery procedures
            removeErlangNode(event.getNodeName());
            System.out.println("Removed node: " + event.getNodeName());
            // Execute recovery logic for the failed node
            recoverFromNodeFailure(event.getNodeName());
        }
    }
}
