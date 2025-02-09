package com.whenly.service;

import com.whenly.model.Event;
import com.whenly.model.User;
import com.whenly.repository.ConstraintRepository;
import com.whenly.repository.EventRepository;
import com.whenly.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;
import com.ericsson.otp.erlang.*;


import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.stream.Collectors;

@Service
public class EventService {

    @Autowired
    private EventRepository eventRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ErlangBackendAPI erlangBackendAPI; // Iniettato per comunicare con Erlang

    @Autowired
    private SharedStringList sharedStringList; // Iniettato per gestire gli indirizzi IP in modo thread-safe

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

        // 📌 Seleziona un nodo Erlang in modo thread-safe
        String assignedErlangNode = selectErlangNode();
        if (assignedErlangNode == null) {
            throw new ResponseStatusException(HttpStatus.SERVICE_UNAVAILABLE, "No available Erlang nodes");
        }

        // 📢 Invia il messaggio al backend Erlang
        System.out.println("Sending event to Erlang backend...");
        boolean success = sendEventToErlang(eventName, parsedDeadline, assignedErlangNode);

        if (!success) {
            throw new ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR, "Failed to notify Erlang backend");
        }

        // 📌 Salviamo l'evento con l'IP del nodo Erlang selezionato
        Event newEvent = new Event(eventName, userOpt.get().getUsername(), parsedDeadline, assignedErlangNode);
        eventRepository.save(newEvent);
        Long eventId = newEvent.getId();

        // 📌 Risposta HTTP
        Map<String, String> response = new HashMap<>();
        response.put("message", "Event created successfully");
        response.put("eventId", String.valueOf(eventId));
        response.put("assignedErlangNode", assignedErlangNode);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    /**
     * Seleziona un nodo Erlang disponibile utilizzando Round Robin.
     * L'accesso alla lista degli indirizzi è thread-safe.
     *
     * @return L'indirizzo IP del nodo selezionato o null se la lista è vuota.
     */
    private String selectErlangNode() {
        List<String> nodes = sharedStringList.getStrings();
        if (nodes.isEmpty()) {
            return null; // Nessun nodo disponibile
        }

        // Round Robin
        synchronized (this) {
            String selectedNode = nodes.get(currentNodeIndex);
            currentNodeIndex = (currentNodeIndex + 1) % nodes.size();
            return selectedNode;
        }
    }

    /**
     * Invia l'evento al cluster Erlang utilizzando ErlangBackendAPI.
     *
     * @param eventName      Nome dell'evento.
     * @param deadline       Scadenza dell'evento.
     * @param assignedErlangNode IP del nodo Erlang assegnato.
     * @return True se l'invio ha avuto successo, false altrimenti.
     */
    public boolean sendEventToErlang(String eventName, LocalDateTime deadline, String assignedErlangNode) {
        try {
            // Converte il deadline in Unix timestamp
            long unixDeadline = deadline.toEpochSecond(java.time.ZoneOffset.UTC);

            OtpErlangList constraints = new OtpErlangList();

            // Usa ErlangBackendAPI per inviare l'evento
            System.out.println("Sending event to Erlang backend: " + eventName);
            erlangBackendAPI.createEvent(assignedErlangNode, eventName, unixDeadline, constraints);
            return true;
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }

    /**
     * Aggiunge un nodo Erlang alla lista condivisa.
     *
     * @param nodeIp Indirizzo IP del nodo da aggiungere.
     */
    public void addErlangNode(String nodeIp) {
        sharedStringList.addString(nodeIp);
    }

    /**
     * Rimuove un nodo Erlang dalla lista condivisa.
     *
     * @param nodeIp Indirizzo IP del nodo da rimuovere.
     */
    public void removeErlangNode(String nodeIp) {
        sharedStringList.removeString(nodeIp);
    }

    public ResponseEntity<List<Map<String, String>>> getEventsByUser(String username) {

        // Implement the logic to retrieve events by user

        // For now, return a placeholder response

        return ResponseEntity.ok(List.of(Map.of("username", username, "event", "sample event")));

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
        response.put("erlangNodeIp", event.getErlangNodeIp());

        return ResponseEntity.ok(response);
    }
}
