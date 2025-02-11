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

public ResponseEntity<Map<String, String>> addConstraint(Long eventId, List<String> constraintsList, String username) {
    // Verifica se l'evento esiste
    Optional<Event> eventOpt = eventRepository.findById(eventId);
    if (eventOpt.isEmpty()) {
        throw new ResponseStatusException(HttpStatus.NOT_FOUND, "Event not found");
    }
    
    Event event = eventOpt.get();
    
    // Parsing e validazione dei vincoli, convertendo le date in timestamp Unix
    List<Map<String, String>> parsedConstraints = new ArrayList<>();
    for (String constraint : constraintsList) {
        try {
            // Splitta la stringa in lowerLimit e upperLimit
            String[] limits = constraint.split(",");
            if (limits.length != 2) {
                throw new IllegalArgumentException("Invalid constraint format. Expected 'lowerLimit,upperLimit'.");
            }
            
            // Parsing delle date usando il formatter (es. DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"))
            LocalDateTime lowerLimit = LocalDateTime.parse(limits[0].trim(), formatter);
            LocalDateTime upperLimit = LocalDateTime.parse(limits[1].trim(), formatter);
            
            if (lowerLimit.isAfter(upperLimit)) {
                throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Lower limit must be before upper limit.");
            }
            
            // Conversione in timestamp Unix (in secondi)
            long lowerEpoch = lowerLimit.toEpochSecond(ZoneOffset.ofHours(1));
            long upperEpoch = upperLimit.toEpochSecond(ZoneOffset.ofHours(1));
      
            parsedConstraints.add(Map.of(
                "lowerLimit", String.valueOf(lowerEpoch),
                "upperLimit", String.valueOf(upperEpoch)
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
        Constraint newConstraint = new Constraint(event, username, assignedNode, parsedConstraints);
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
    public boolean sendConstraintsToErlang(Long eventId, List<Map<String, String>> constraintsList, String assignedNode) {
        System.out.println("Questi sono i constraintsList: " + constraintsList);
        
        try {
            // Costruisce la lista dei vincoli come lista Erlang, convertendo i valori in OtpErlangLong
            OtpErlangList erlangConstraints = new OtpErlangList(
                constraintsList.stream()
                    .map(constraint -> {
                        // Recupera i timestamp Unix precedentemente salvati
                        long lowerLimit = Long.parseLong(constraint.get("lowerLimit"));
                        long upperLimit = Long.parseLong(constraint.get("upperLimit"));

                        // Crea un tuple Erlang {lowerLimit, upperLimit}
                        return new OtpErlangTuple(new OtpErlangObject[]{
                            new OtpErlangLong(lowerLimit),
                            new OtpErlangLong(upperLimit)
                        });
                    })
                    .toArray(OtpErlangObject[]::new)
            );
            
            // Invia il messaggio al nodo Erlang
            erlangBackendAPI.addConstraint(assignedNode, String.valueOf(eventId), erlangConstraints);
            return true;
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }
    
}
