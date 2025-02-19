package com.whenly.model;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.persistence.*;

import java.util.List;
import java.util.Map;

@Entity
@Table(name = "constraints")
public class Constraint {

    // Unique identifier for the constraint
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    // The event associated with the constraint
    @ManyToOne
    @JoinColumn(name = "event_id", nullable = false)
    private Event event;

    // The username of the person who created the constraint
    @Column(nullable = false, length = 8)
    private String username;

    // The Erlang node assigned to this constraint
    @Column(nullable = false, length = 255)
    private String assignedErlangNode;

    // JSON string to store the list of constraints
    @Column(nullable = false, columnDefinition = "json")
    private String constraintsList;

    // ObjectMapper instance for JSON serialization/deserialization
    @Transient
    private static final ObjectMapper objectMapper = new ObjectMapper();

    // Default constructor
    public Constraint() {}

    // Constructor to initialize all fields
    public Constraint(Event event, String username, String assignedErlangNode, List<Map<String, String>> constraintsList) throws JsonProcessingException {
        this.event = event;
        this.username = username;
        this.assignedErlangNode = assignedErlangNode;
        this.constraintsList = serializeConstraints(constraintsList); // Serialize the constraints
    }

    // Getter and Setter methods
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

    // Method to serialize the list of constraints to a JSON string
    private String serializeConstraints(List<Map<String, String>> constraintsList) throws JsonProcessingException {
        return objectMapper.writeValueAsString(constraintsList);
    }

    // Method to deserialize the JSON string to a list of constraints
    public static List<Map<String, String>> deserializeConstraints(String constraintsList) throws JsonProcessingException {
        return objectMapper.readValue(constraintsList, List.class);
    }
}