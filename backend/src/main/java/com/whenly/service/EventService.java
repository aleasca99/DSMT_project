package com.whenly.service;

import com.whenly.model.Event;
import com.whenly.model.User;
import com.whenly.repository.ConstraintRepository;
import com.whenly.repository.EventRepository;
import com.whenly.repository.UserRepository;
//import com.ericsson.otp.erlang.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;

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
    private ConstraintRepository constraintRepository;

    private final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");

    // 📌 Lista di nodi Erlang disponibili (IP delle macchine)
    private final String[] erlangNodes = {
            "192.168.1.101",
            "192.168.1.102",
            "192.168.1.103"
    };
    private int currentNodeIndex = 0; // Indice per Round Robin

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

        // 📌 Seleziona un nodo Erlang con Round Robin
        String assignedErlangNode = erlangNodes[currentNodeIndex];
        currentNodeIndex = (currentNodeIndex + 1) % erlangNodes.length;

        // 📢 Invia il messaggio al backend Erlang
        boolean success = true;//;sendEventToErlang(eventName, parsedDeadline, assignedErlangNode);

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


    public boolean sendEventToErlang(Long eventId, LocalDateTime deadline) {
        return true;
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
        response.put("erlangNodeIp", event.getErlangNodeIp()); // 📌 Aggiunto l'IP del nodo Erlang

        return ResponseEntity.ok(response);
    }

    public ResponseEntity<List<Map<String, String>>> getEventsByUser(String username) {
        List<Event> events = constraintRepository.findEventsByUsername(username);

        // Converte la lista di eventi in una lista di mappe contenenti solo le informazioni necessarie
        List<Map<String, String>> response = events.stream()
                .map(event -> Map.of(
                        "eventName", event.getEventName(),
                        "creator", event.getCreatorUsername(),
                        "result", event.getFinalResult() != null ? event.getFinalResult() : event.getDeadline().toString(),
                        "erlangNodeIp", event.getErlangNodeIp() // 📌 Aggiunto l'IP del nodo Erlang
                ))
                .collect(Collectors.toList());

        return ResponseEntity.ok(response);
    }
}
