package com.whenly.controller;

import com.whenly.service.EventService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/api/events")
@Tag(name = "Events", description = "Event management APIs")
public class EventController {

    @Autowired
    private EventService eventService;

    @Operation(summary = "Create a new event", description = "Creates a new event with a name, deadline, and an associated user.")
    @PostMapping("/create")
    public ResponseEntity<Map<String, String>> createEvent(
            @Parameter(name = "eventName", description = "Name of the event", required = true)
            @RequestParam String eventName,

            @Parameter(name = "deadline", description = "Deadline for the event (format: YYYY-MM-DD HH:mm)", required = true)
            @RequestParam String deadline,

            @Parameter(name = "username", description = "Username of the event creator", required = true)
            @RequestParam String username) {

        return eventService.createEvent(eventName, deadline, username);
    }

    @Operation(summary = "Get event details", description = "Retrieves the details of an event given its ID.")
    @GetMapping("/{eventId}")
    public ResponseEntity<Map<String, String>> getEvent(
            @Parameter(name = "eventId", description = "ID of the event to retrieve", required = true)
            @PathVariable Long eventId) {

        return eventService.getEventById(eventId);
    }

    @Operation(summary = "Get events where user has added a constraint",
            description = "Retrieves all events where the user has added constraints, including the event name, creator username, and result.")
    @GetMapping("/user/{username}")
    public ResponseEntity<List<Map<String, String>>> getUserEvents(
            @Parameter(name = "username", description = "Username of the user to retrieve events for", required = true)
            @PathVariable String username) {

        return eventService.getEventsByUser(username);
    }
}
