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

    public ResponseEntity<Map<String, String>> addConstraint(Long eventId, List<Map<String, String>> constraintsList, String username) {
        // Verifica se l'evento esiste
        Optional<Event> eventOpt = eventRepository.findById(eventId);
        if (eventOpt.isEmpty()) {
            throw new ResponseStatusException(HttpStatus.NOT_FOUND, "Event not found");
        }

        // Parsing e validazione dei vincoli
        List<Map<String, String>> parsedConstraints = new ArrayList<>();
        for (Map<String, String> constraint : constraintsList) {
            try {
                LocalDateTime lowerLimit = LocalDateTime.parse(constraint.get("lowerLimit"), formatter);
                LocalDateTime upperLimit = LocalDateTime.parse(constraint.get("upperLimit"), formatter);
                if (lowerLimit.isAfter(upperLimit)) {
                    throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Lower limit must be before upper limit.");
                }
                parsedConstraints.add(Map.of(
                    "lowerLimit", lowerLimit.toString(),
                    "upperLimit", upperLimit.toString()
                ));
            } catch (Exception e) {
                throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Invalid date format. Use 'YYYY-MM-DD HH:mm'");
            }
        }

        // Seleziona un nodo Erlang disponibile
        String assignedNode = selectErlangNode();
        if (assignedNode == null) {
            throw new ResponseStatusException(HttpStatus.SERVICE_UNAVAILABLE, "No available Erlang nodes");
        }

        // Invia i vincoli al nodo Erlang
        boolean sentSuccessfully = sendConstraintsToErlang(eventId, parsedConstraints, assignedNode);

        if (!sentSuccessfully) {
            throw new ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR, "Failed to communicate with Erlang node");
        }

        // Salva i vincoli nel database come JSON
        try {
            Constraint newConstraint = new Constraint(eventOpt.get(), username, assignedNode, parsedConstraints);
            constraintRepository.save(newConstraint);
        } catch (Exception e) {
            throw new ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR, "Failed to save constraints to database");
        }

        // Prepara la risposta
        Map<String, String> response = new HashMap<>();
        response.put("message", "Constraints added successfully");
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
     * Invia i vincoli al cluster Erlang tramite ErlangBackendAPI.
     *
     * @param eventId         ID dell'evento.
     * @param constraintsList Lista dei vincoli.
     * @param assignedNode    Nodo Erlang assegnato.
     * @return True se l'invio ha avuto successo, false altrimenti.
     */
    private boolean sendConstraintsToErlang(Long eventId, List<Map<String, String>> constraintsList, String assignedNode) {
        try {
            // Costruisce la lista dei vincoli come Erlang list
            OtpErlangList erlangConstraints = new OtpErlangList(
                constraintsList.stream()
                    .map(constraint -> {
                        try {
                            long lowerLimit = LocalDateTime.parse(constraint.get("lowerLimit"), formatter).toEpochSecond(java.time.ZoneOffset.UTC);
                            long upperLimit = LocalDateTime.parse(constraint.get("upperLimit"), formatter).toEpochSecond(java.time.ZoneOffset.UTC);
                            return new OtpErlangTuple(new OtpErlangObject[]{
                                new OtpErlangLong(lowerLimit),
                                new OtpErlangLong(upperLimit)
                            });
                        } catch (Exception e) {
                            throw new RuntimeException("Error parsing constraints for Erlang: " + e.getMessage());
                        }
                    })
                    .toArray(OtpErlangObject[]::new)
            );

            // Invia al nodo Erlang
            erlangBackendAPI.addConstraint(assignedNode, String.valueOf(eventId), erlangConstraints);
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
