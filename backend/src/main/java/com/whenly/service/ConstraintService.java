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
import java.time.format.DateTimeFormatter;
import java.util.*;

@Service
public class ConstraintService {

    @Autowired
    private ConstraintRepository constraintRepository;

    @Autowired
    private EventRepository eventRepository;

    @Autowired
    private SharedStringList sharedStringList; // Per la lista condivisa degli IP

    @Autowired
    private ErlangBackendAPI erlangBackendAPI; // Per la comunicazione con Erlang

    private final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
    private int currentNodeIndex = 0; // Per la selezione Round Robin

    public ResponseEntity<Map<String, String>> addConstraint(Long eventId, String lowerLimit, String upperLimit, String username) {
        // Verifica se l'evento esiste
        Optional<Event> eventOpt = eventRepository.findById(eventId);
        if (eventOpt.isEmpty()) {
            throw new ResponseStatusException(HttpStatus.NOT_FOUND, "Event not found");
        }

        // Parsing delle date
        LocalDateTime parsedLowerLimit, parsedUpperLimit;
        try {
            parsedLowerLimit = LocalDateTime.parse(lowerLimit, formatter);
            parsedUpperLimit = LocalDateTime.parse(upperLimit, formatter);
        } catch (Exception e) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Invalid date format. Use 'YYYY-MM-DD HH:mm'");
        }

        // Seleziona un nodo Erlang disponibile
        String assignedNode = selectErlangNode();
        if (assignedNode == null) {
            throw new ResponseStatusException(HttpStatus.SERVICE_UNAVAILABLE, "No available Erlang nodes");
        }

        // Invia il vincolo al nodo Erlang
        boolean sentSuccessfully = sendConstraintToErlang(eventId, parsedLowerLimit, parsedUpperLimit, assignedNode);

        if (!sentSuccessfully) {
            throw new ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR, "Failed to communicate with Erlang node");
        }

        // Salva il vincolo nel database
        Constraint newConstraint = new Constraint(eventOpt.get(), parsedLowerLimit, parsedUpperLimit, username, assignedNode);
        constraintRepository.save(newConstraint);

        // Prepara la risposta
        Map<String, String> response = new HashMap<>();
        response.put("message", "Constraint added successfully");
        response.put("assignedNode", assignedNode);
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
     * Invia un vincolo al cluster Erlang tramite ErlangBackendAPI.
     *
     * @param eventId      ID dell'evento.
     * @param lowerLimit   Limite inferiore del vincolo.
     * @param upperLimit   Limite superiore del vincolo.
     * @param assignedNode Nodo Erlang assegnato.
     * @return True se l'invio ha avuto successo, false altrimenti.
     */
    private boolean sendConstraintToErlang(Long eventId, LocalDateTime lowerLimit, LocalDateTime upperLimit, String assignedNode) {
        try {
            // Converte i limiti in formato Unix timestamp
            long unixLowerLimit = lowerLimit.toEpochSecond(java.time.ZoneOffset.UTC);
            long unixUpperLimit = upperLimit.toEpochSecond(java.time.ZoneOffset.UTC);

            // Costruisce la lista dei vincoli come Erlang tuple
            OtpErlangObject[] constraintTuple = new OtpErlangObject[]{
                new OtpErlangLong(unixLowerLimit),
                new OtpErlangLong(unixUpperLimit)
            };
            OtpErlangList constraints = new OtpErlangList(new OtpErlangObject[]{new OtpErlangTuple(constraintTuple)});

            // Invoca il metodo di ErlangBackendAPI
            erlangBackendAPI.addConstraint(assignedNode, String.valueOf(eventId), constraints);
            return true;
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }

    public List<Constraint> getUnfinishedConstraintsByNode(String erlangNode) {
        return constraintRepository.findUnfinishedConstraintsByNode(erlangNode);
    }
    
}
