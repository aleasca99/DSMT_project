package com.whenly.controller;

import com.whenly.service.ConstraintService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/api/constraints")
@Tag(name = "Constraints", description = "Constraint management APIs")
public class ConstraintController {

    @Autowired
    private ConstraintService constraintService;

    @Operation(
            summary = "Add constraints to an event",
            description = "Adds a list of constraints for an event. Each constraint should be a string formatted as 'lowerLimit,upperLimit'."
    )
    @PostMapping("/add")
    public ResponseEntity<Map<String, String>> addConstraints(
            @Parameter(name = "eventId", description = "ID of the event", required = true)
            @RequestParam Long eventId,

            @Parameter(name = "constraints", description = "List of constraints, each formatted as 'lowerLimit,upperLimit'", required = true, example = "[\"2022-01-01 12:00,2022-01-01 14:00\"]")
            @RequestBody List<String> constraints,

            @Parameter(name = "username", description = "Username of the constraint submitter", required = true)
            @RequestParam String username) {

        return constraintService.addConstraint(eventId, constraints, username);
    }
}
