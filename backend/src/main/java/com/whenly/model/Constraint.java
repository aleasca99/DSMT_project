package com.whenly.model;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.persistence.*;

import java.util.List;
import java.util.Map;

@Entity
@Table(name = "constraints")
public class Constraint {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne
    @JoinColumn(name = "event_id", nullable = false)
    private Event event;

    @Column(nullable = false, length = 8)
    private String username;

    @Column(nullable = false, length = 255)
    private String assignedErlangNode;

    @Column(nullable = false, columnDefinition = "json")
    private String constraintsList; // JSON string to store the list of constraints

    @Transient
    private static final ObjectMapper objectMapper = new ObjectMapper();

    public Constraint() {}

    public Constraint(Event event, String username, String assignedErlangNode, List<Map<String, String>> constraintsList) throws JsonProcessingException {
        this.event = event;
        this.username = username;
        this.assignedErlangNode = assignedErlangNode;
        this.constraintsList = serializeConstraints(constraintsList); // Serializza i vincoli
    }

    // Getter e Setter
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Event getEvent() {
        return event;
    }

    public void setEvent(Event event) {
        this.event = event;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getAssignedErlangNode() {
        return assignedErlangNode;
    }

    public void setAssignedErlangNode(String assignedErlangNode) {
        this.assignedErlangNode = assignedErlangNode;
    }

    public String getConstraintsList() {
        return constraintsList;
    }

    public void setConstraintsList(String constraintsList) {
        this.constraintsList = constraintsList;
    }

    // **Helper per la gestione dei vincoli come oggetti Java**
    public List<Map<String, String>> getConstraintsAsList() throws JsonProcessingException {
        return deserializeConstraints(this.constraintsList);
    }

    public void setConstraintsAsList(List<Map<String, String>> constraintsList) throws JsonProcessingException {
        this.constraintsList = serializeConstraints(constraintsList);
    }

    // Helper per serializzare la lista di vincoli in JSON
    private static String serializeConstraints(List<Map<String, String>> constraints) throws JsonProcessingException {
        return objectMapper.writeValueAsString(constraints);
    }

    // Helper per deserializzare il JSON in una lista di vincoli
    private static List<Map<String, String>> deserializeConstraints(String json) throws JsonProcessingException {
        return objectMapper.readValue(json, List.class);
    }

    // Getters and setters
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Event getEvent() {
        return event;
    }

    public void setEvent(Event event) {
        this.event = event;
    }

    public LocalDateTime getLowerLimit() {
        return lowerLimit;
    }

    public void setLowerLimit(LocalDateTime lowerLimit) {
        this.lowerLimit = lowerLimit;
    }

    public LocalDateTime getUpperLimit() {
        return upperLimit;
    }

    public void setUpperLimit(LocalDateTime upperLimit) {
        this.upperLimit = upperLimit;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getAssignedErlangNode() {
        return assignedErlangNode;
    }

    public void setAssignedErlangNode(String assignedErlangNode) {
        this.assignedErlangNode = assignedErlangNode;
    }

    public void setErlangNode(String erlangNode) {
        this.assignedErlangNode = erlangNode;
    }

    public String getErlangNode() {
        return assignedErlangNode;
    }
}
