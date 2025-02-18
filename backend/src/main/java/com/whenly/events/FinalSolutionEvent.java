package com.whenly.events;

public class FinalSolutionEvent {
    // The ID of the event
    private final String eventId;
    
    // The solution associated with the event
    private final String solution;

    // Constructor to initialize the eventId and solution
    public FinalSolutionEvent(String eventId, String solution) {
        this.eventId = eventId;
        this.solution = solution;
    }

    // Getter method for eventId
    public String getEventId() {
        return eventId;
    }

    // Getter method for solution
    public String getSolution() {
        return solution;
    }
}