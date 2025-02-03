package com.whenly.controller;

import com.whenly.model.Event;
import com.whenly.service.EventService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/events")
@Tag(name = "Events", description = "Gestione degli eventi")
public class EventController {

    @Autowired
    private EventService eventService;

    @Operation(summary = "Ottieni tutti gli eventi")
    @GetMapping
    public ResponseEntity<List<Event>> getEvents() {
        return ResponseEntity.ok(eventService.getAllEvents());
    }

    @Operation(summary = "Crea un nuovo evento")
    @PostMapping("/create")
    public ResponseEntity<Event> createEvent(@RequestBody Event event) {
        return ResponseEntity.ok(eventService.createEvent(event));
    }
}
