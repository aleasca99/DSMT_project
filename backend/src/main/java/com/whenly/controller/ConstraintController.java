package com.whenly.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.Map;

@RestController
@RequestMapping("/api/constraints")
@Tag(name = "Constraints", description = "Constraint management APIs")
public class ConstraintController {

    @Operation(summary = "Add a constraint to an event",
            description = "Adds a scheduling constraint for a specific event.")
    @PostMapping("/add")
    public ResponseEntity<Map<String, String>> addConstraint(
            @Parameter(name = "lowerLimit", description = "Lower time limit for the constraint. Format should be a timestamp", required = true)
            @RequestParam String lowerLimit,

            @Parameter(name = "upperLimit", description = "Upper time limit for the constraint. Format should be a timestamp", required = true)
            @RequestParam String upperLimit,

            @Parameter(name = "username", description = "User who is adding the constraint", required = true)
            @RequestParam String username,

            @Parameter(name = "eventId", description = "ID of the event to which the constraint applies", required = true)
            @RequestParam Long eventId) {

        Map<String, String> response = new HashMap<>();
        response.put("message", "Constraint added successfully");
        return ResponseEntity.ok(response);
    }

    @Operation(summary = "Compute the final schedule",
            description = "Computes the final schedule for an event (Only executable from cluster nodes).")
    @PostMapping("/compute")
    public ResponseEntity<Map<String, String>> computeFinalSchedule(
            @Parameter(name = "eventId", description = "ID of the event to be computed", required = true)
            @RequestParam Long eventId,

            @Parameter(name = "lowerLimit", description = "Lower time limit for computation. Format should be a timestamp", required = true)
            @RequestParam String lowerLimit,

            @Parameter(name = "upperLimit", description = "Upper time limit for computation. Format should be a timestamp", required = true)
            @RequestParam String upperLimit) {

        Map<String, String> response = new HashMap<>();
        response.put("message", "Final schedule computed successfully");
        return ResponseEntity.ok(response);
    }
}
