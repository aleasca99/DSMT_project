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

        Event newEvent = new Event(eventName, userOpt.get().getUsername(), parsedDeadline);
        eventRepository.save(newEvent);

        Map<String, String> response = new HashMap<>();
        response.put("message", "Event created successfully");
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
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

        return ResponseEntity.ok(response);
    }

    public ResponseEntity<List<Map<String, String>>> getEventsByUser(String username) {
        List<Event> events = constraintRepository.findEventsByUsername(username);

        // Converte la lista di eventi in una lista di mappe contenenti solo le informazioni necessarie
        List<Map<String, String>> response = events.stream()
                .map(event -> Map.of(
                        "eventName", event.getEventName(),
                        "creator", event.getCreatorUsername(),
                        "result", event.getFinalResult() != null ? event.getFinalResult() : event.getDeadline().toString()
                ))
                .collect(Collectors.toList());

        return ResponseEntity.ok(response);
    }
}
