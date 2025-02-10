package com.whenly.events;

public class FinalSolutionEvent {
    private final String eventId;
    private final String solution;

    public FinalSolutionEvent(String eventId, String solution) {
        this.eventId = eventId;
        this.solution = solution;
    }

    public String getEventId() {
        return eventId;
    }

    public String getSolution() {
        return solution;
    }
}
