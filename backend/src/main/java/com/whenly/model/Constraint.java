package com.whenly.model;

import jakarta.persistence.*;
import java.time.LocalDateTime;

@Entity
@Table(name = "constraints")
public class Constraint {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne
    @JoinColumn(name = "event_id", nullable = false)
    private Event event;

    @Column(nullable = false)
    private LocalDateTime lowerLimit;

    @Column(nullable = false)
    private LocalDateTime upperLimit;

    @Column(nullable = false, length = 8)
    private String username;

    @Column(nullable = false, length = 255)
    private String assignedErlangNode;

    public Constraint() {}

    public Constraint(Event event, LocalDateTime lowerLimit, LocalDateTime upperLimit, String username, String assignedErlangNode) {
        this.event = event;
        this.lowerLimit = lowerLimit;
        this.upperLimit = upperLimit;
        this.username = username;
        this.assignedErlangNode = assignedErlangNode;
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
