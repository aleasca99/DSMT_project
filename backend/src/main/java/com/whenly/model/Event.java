package com.whenly.model;

import jakarta.persistence.*;
import java.time.LocalDateTime;

@Entity
@Table(name = "event")
public class Event {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "event_name", nullable = false)
    private String eventName;

    @Column(name = "creator_username", nullable = false, length = 8)
    private String creatorUsername;

    @Column(name = "deadline", nullable = false)
    private LocalDateTime deadline;

    @Column(name = "final_result")
    private String finalResult;

    @Column(name = "erlang_node_ip", nullable = false, length = 45)
    private String erlangNodeIp; // Nuovo campo per l'IP del nodo Erlang

    // Costruttore senza ID (Hibernate lo genera automaticamente)
    public Event(String eventName, String creatorUsername, LocalDateTime deadline, String erlangNodeIp) {
        this.eventName = eventName;
        this.creatorUsername = creatorUsername;
        this.deadline = deadline;
        this.erlangNodeIp = erlangNodeIp;
    }

    public Event() {
    }

    // Getter e Setter
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

    public String getErlangNodeIp() {
        return erlangNodeIp;
    }

    public void setFinalSolution(String finalResult) {
        this.finalResult = finalResult;
    }

    public void setErlangNodeIp(String erlangNodeIp) {
        this.erlangNodeIp = erlangNodeIp;
    }
}
