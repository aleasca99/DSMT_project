package com.whenly.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.Map;

@RestController
@RequestMapping("/api/events")
@Tag(name = "Events", description = "Event management APIs")
public class EventController {

    @Operation(summary = "Create a new event", description = "Creates a new event with a deadline and an associated user.")
    @PostMapping("/create")
    public ResponseEntity<Map<String, String>> createEvent(
            @Parameter(name = "deadline", description = "Deadline for the event (format: YYYY-MM-DD HH:mm)", required = true)
            @RequestParam String deadline,

            @Parameter(name = "username", description = "Username of the event creator", required = true)
            @RequestParam String username) {

        Map<String, String> response = new HashMap<>();
        response.put("message", "Event created successfully");
        return ResponseEntity.ok(response);
    }

    @Operation(summary = "Get event details", description = "Retrieves the details of an event given its ID.")
    @GetMapping("/{eventId}")
    public ResponseEntity<Map<String, String>> getEvent(
            @Parameter(name = "eventId", description = "ID of the event to retrieve", required = true)
            @PathVariable Long eventId) {

        Map<String, String> response = new HashMap<>();
        response.put("message", "Event retrieved successfully");
        response.put("eventId", String.valueOf(eventId));
        return ResponseEntity.ok(response);
    }
}
