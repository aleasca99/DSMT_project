package com.whenly.service;

import com.whenly.model.Constraint;
import com.whenly.model.Event;
import com.whenly.repository.ConstraintRepository;
import com.whenly.repository.EventRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;

import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.Map;
import java.io.OutputStream;
import java.util.Optional;

@Service
public class ConstraintService {

    @Autowired
    private ConstraintRepository constraintRepository;

    @Autowired
    private EventRepository eventRepository;

    private final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");

    private final String[] erlangNodes = {
            "http://192.168.1.101:4000/api/receive_constraint",
            "http://192.168.1.102:4000/api/receive_constraint",
            "http://192.168.1.103:4000/api/receive_constraint"
    };
    private int currentNodeIndex = 0;

    public ResponseEntity<Map<String, String>> addConstraint(Long eventId, String lowerLimit, String upperLimit, String username) {
        // Verifica se l'evento esiste
        Optional<Event> eventOpt = eventRepository.findById(eventId);
        if (eventOpt.isEmpty()) {
            throw new ResponseStatusException(HttpStatus.NOT_FOUND, "Event not found");
        }

        // Verifica se il vincolo esiste già per lo stesso utente e evento
        if (constraintRepository.existsByEventIdAndUsername(eventId, username)) {
            throw new ResponseStatusException(HttpStatus.CONFLICT, "A constraint for this event and user already exists");
        }

        // Parsing delle date
        LocalDateTime parsedLowerLimit, parsedUpperLimit;
        try {
            parsedLowerLimit = LocalDateTime.parse(lowerLimit, formatter);
            parsedUpperLimit = LocalDateTime.parse(upperLimit, formatter);
        } catch (Exception e) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Invalid date format. Use 'YYYY-MM-DD HH:mm'");
        }

        // Seleziona la macchina Erlang con bilanciamento circolare
        String assignedNode = erlangNodes[currentNodeIndex];
        currentNodeIndex = (currentNodeIndex + 1) % erlangNodes.length;

        // Invia il vincolo alla macchina Erlang
        boolean sentSuccessfully = sendConstraintToErlang(eventId, parsedLowerLimit, parsedUpperLimit, assignedNode);

        if (!sentSuccessfully) {
            throw new ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR, "Failed to communicate with Erlang node");
        }

        // Salva il vincolo nel database
        Constraint newConstraint = new Constraint(eventOpt.get(), parsedLowerLimit, parsedUpperLimit, username, assignedNode);
        constraintRepository.save(newConstraint);

        Map<String, String> response = new HashMap<>();
        response.put("message", "Constraint added successfully");
        response.put("assignedNode", assignedNode);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    private boolean sendConstraintToErlang(Long eventId, LocalDateTime lowerLimit, LocalDateTime upperLimit, String erlangUrl) {
        try {
            URL url = new URL(erlangUrl);
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("POST");
            connection.setRequestProperty("Content-Type", "application/json");
            connection.setDoOutput(true);

            String jsonPayload = String.format(
                    "{\"eventId\": %d, \"lowerLimit\": \"%s\", \"upperLimit\": \"%s\"}",
                    eventId, lowerLimit.toString(), upperLimit.toString()
            );

            try (OutputStream os = connection.getOutputStream()) {
                byte[] input = jsonPayload.getBytes(StandardCharsets.UTF_8);
                os.write(input, 0, input.length);
            }

            int responseCode = connection.getResponseCode();
            return responseCode == 200;

        } catch (Exception e) {
            return false;
        }
    }
}
