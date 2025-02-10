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
}
