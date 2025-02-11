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

    @Autowired
    private EventRepository eventRepository;

    @Autowired
    private ConstraintRepository constraintRepository;

    @Autowired
    private UserRepository userRepository;


    @Autowired
    private ConstraintService constraintService; // Iniettato per poter usare la funzione di invio


    // Se necessario, EventService può ancora chiamare ErlangBackendAPI per inviare messaggi
    @Autowired
    private ErlangBackendAPI erlangBackendAPI;

    @Autowired
    private SharedStringList sharedStringList;

    private final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");

    // Indice per la selezione Round Robin
    private int currentNodeIndex = 0;

    public ResponseEntity<Map<String, String>> createEvent(String eventName, String deadline, String username) {
        Optional<User> userOpt = userRepository.findByUsername(username);
        if (userOpt.isEmpty()) {
            throw new ResponseStatusException(HttpStatus.NOT_FOUND, "User not found");
        }
        LocalDateTime parsedDeadline;
        try {
            parsedDeadline = LocalDateTime.parse(deadline, formatter);
        } catch (Exception e) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Invalid deadline format. Use 'YYYY-MM-DD HH:mm'");
        }
        String assignedErlangNode = selectErlangNode();
        if (assignedErlangNode == null) {
            throw new ResponseStatusException(HttpStatus.SERVICE_UNAVAILABLE, "No available Erlang nodes");
        }
        Event newEvent = new Event(eventName, userOpt.get().getUsername(), parsedDeadline, assignedErlangNode);
        eventRepository.save(newEvent);
        Long eventId = newEvent.getId();
        System.out.println("Event created successfully with ID: " + eventId);
        System.out.println("Sending event to Erlang backend...");
        boolean success = sendEventToErlang(eventId, parsedDeadline, assignedErlangNode);
        if (!success) {
            throw new ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR, "Failed to notify Erlang backend");
        }
        Map<String, String> response = new HashMap<>();
        response.put("message", "Event created successfully");
        response.put("eventId", String.valueOf(eventId));
        response.put("assignedErlangNode", assignedErlangNode);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    private String selectErlangNode() {
        List<String> nodes = sharedStringList.getStrings();
        if (nodes.isEmpty()) {
            return null;
        }
        synchronized (this) {
            String selectedNode = nodes.get(currentNodeIndex);
            currentNodeIndex = (currentNodeIndex + 1) % nodes.size();
            return selectedNode;
        }
    }

    public boolean sendEventToErlang(Long eventID, LocalDateTime deadline, String assignedErlangNode) {
        try {
            long unixDeadline = deadline.toEpochSecond(ZoneOffset.ofHours(1));
            OtpErlangList constraints = new OtpErlangList();
            System.out.println("Sending event to Erlang backend: " + eventID.toString());
            erlangBackendAPI.createEvent(assignedErlangNode, eventID.toString(), unixDeadline, constraints);
            return true;
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }

    public void addErlangNode(String nodeIp) {
        sharedStringList.addString(nodeIp);
    }

    public void removeErlangNode(String nodeIp) {
        sharedStringList.removeString(nodeIp);
    }


    public void updateFinalSolution(Long eventId, String finalSolution) {
        Optional<Event> eventOpt = eventRepository.findById(eventId);
        if (eventOpt.isEmpty()) {
            throw new ResponseStatusException(HttpStatus.NOT_FOUND, "Event not found");
        }
        String trimmedSolution = finalSolution.trim();
        if (trimmedSolution.startsWith("[{") && trimmedSolution.endsWith("}]")) {
            trimmedSolution = trimmedSolution.substring(1, trimmedSolution.length() - 1);
        }
        String[] parts = trimmedSolution.split(",");
        if (parts.length < 2) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Invalid finalSolution tuple format. Expected two elements.");
        }
        String firstElement = parts[0].trim().replaceAll("[\\[\\{\\]\\}]", "");
        String secondElement = parts[1].trim().replaceAll("[\\[\\{\\]\\}]", "");

        System.err.println("First element: " + firstElement);
        System.err.println("Second element: " + secondElement);

        //se il primo elemento è undefined, aggiungi solo undefined al db
        if (firstElement.equals("undefined")) {
            Event event = eventOpt.get();
            event.setFinalSolution(firstElement);
            eventRepository.save(event);
            return;
        }
        //altrimenti aggiungi entrambi gli elementi come stringa e senza [{


        // Esempio di controllo; puoi aggiungere logica specifica in base al valore di firstElement
        String solutionToInsert = firstElement + ", " + secondElement;
        Event event = eventOpt.get();
        event.setFinalSolution(solutionToInsert);
        eventRepository.save(event);
    }

      /**
     * Restituisce la lista degli eventi a cui l'utente ha aggiunto constraint.
     */

     public ResponseEntity<List<Map<String, String>>> getEventsByUser(String username) {
        // Recupera gli ID (come Number) dalla tabella dei constraint
        List<BigInteger> rawIds = constraintRepository.findEventIdsByUsername(username);
        if (rawIds == null || rawIds.isEmpty()) {
            return ResponseEntity.ok(Collections.emptyList());
        }
        
        // Converti in List<Long>
        List<Long> eventIds = rawIds.stream()
                                    .map(BigInteger::longValue)
                                    .collect(Collectors.toList());
        
        // Recupera gli eventi corrispondenti
        List<Event> events = eventRepository.findAllById(eventIds);
        
        // Mappa ogni evento in una struttura Map<String, String> da restituire
        List<Map<String, String>> eventMaps = new ArrayList<>();
        for (Event event : events) {
            Map<String, String> map = new HashMap<>();
            map.put("eventId", String.valueOf(event.getId()));
            map.put("eventName", event.getEventName());
            map.put("creator", event.getCreatorUsername());
            map.put("deadline", event.getDeadline().toString());
            map.put("erlangNodeIp", event.getErlangNodeIp());
            eventMaps.add(map);
        }
        
        return ResponseEntity.ok(eventMaps);
    }

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
        //response.put("erlangNodeIp", event.getErlangNodeIp());
        response.put("finalSolution", event.getFinalResult());
        return ResponseEntity.ok(response);
    }

    public void recoverFromNodeFailure(String failedNode) {
        List<Constraint> constraintsToRecover = constraintRepository.findConstraintsByErlangNode(failedNode);
        List<String> activeNodes = sharedStringList.getStrings();
        if (activeNodes.isEmpty()) {
            throw new IllegalStateException("No active Erlang nodes available for recovery.");
        }
        for (Constraint constraint : constraintsToRecover) {
            String newNode = selectErlangNode();
            if (newNode == null) {
                throw new IllegalStateException("No available Erlang nodes for redistribution.");
            }
            constraint.setAssignedErlangNode(newNode);
            constraintRepository.save(constraint);
            // Qui puoi chiamare direttamente la funzione di invio definita in ConstraintService:
            try {
                List<Map<String, String>> constraintsAsList = constraint.getConstraintsAsList();
                boolean sent = constraintService.sendConstraintsToErlang(
                        constraint.getEvent().getId(),
                        constraintsAsList,
                        newNode
                );
                if (!sent) {
                    // Gestisci il caso in cui l'invio non sia andato a buon fine
                    System.err.println("Failed to resend constraints for event " + constraint.getEvent().getId());
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        
        // Recovery per gli eventi
        List<Event> eventsToRecover = eventRepository.findByErlangNodeIpAndFinalResultIsNull(failedNode);
        for (Event event : eventsToRecover) {
            String newManagerNode = selectErlangNode();
            if (newManagerNode == null) {
                throw new IllegalStateException("No available Erlang nodes to assign as new manager.");
            }
            event.setErlangNodeIp(newManagerNode);
            eventRepository.save(event);
            erlangBackendAPI.createEvent(newManagerNode, event.getId().toString(),
                    event.getDeadline().toEpochSecond(ZoneOffset.UTC), new OtpErlangList());
        }
    }
    // --- METODI DI ASCOLTO DEGLI EVENTI PUBBLICATI DA ErlangBackendAPI ---

    @EventListener
    public void handleFinalSolutionEvent(FinalSolutionEvent event) {
        System.out.println("EventService received FinalSolutionEvent for EventId: " 
                           + event.getEventId() + ", Solution: " + event.getSolution());
        updateFinalSolution(Long.parseLong(event.getEventId()), event.getSolution());
    }

    @EventListener
    public void handleNodeStatusEvent(NodeStatusEvent event) {
        System.out.println("EventService received NodeStatusEvent for Node: " 
                           + event.getNodeName() + ", Status: " + event.getStatus());
        if ("up".equals(event.getStatus())) {
            addErlangNode(event.getNodeName());
        } else if ("down".equals(event.getStatus())) {
            removeErlangNode(event.getNodeName());
            // Aggiungi eventuale logica di recovery
            recoverFromNodeFailure(event.getNodeName());
        }
    }
}
