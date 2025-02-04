package com.whenly.model;

import jakarta.persistence.*;
import java.time.LocalDateTime;

@Entity
@Table(name = "events")
public class Event {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "event_name", nullable = false)
    private String eventName;

    @Column(name = "creator_username", nullable = false)
    private String creatorUsername;

    @Column(name = "deadline", nullable = false)
    private LocalDateTime deadline;

    @Column(name = "final_result", nullable = true)
    private String finalResult;

    public Event() {}

    public Event(String eventName, String creatorUsername, LocalDateTime deadline) {
        this.eventName = eventName;
        this.creatorUsername = creatorUsername;
        this.deadline = deadline;
    }

    public Long getId() {
        return id;
    }

    public String getEventName() {
        return eventName;
    }

    public String getCreatorUsername() {
        return creatorUsername;
    }

    public LocalDateTime getDeadline() {
        return deadline;
    }

    public String getFinalResult() {
        return finalResult;
    }

    public void setFinalResult(String finalResult) {
        this.finalResult = finalResult;
    }
}
