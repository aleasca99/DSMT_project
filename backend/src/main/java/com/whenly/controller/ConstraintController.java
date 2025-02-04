package com.whenly.controller;

import com.whenly.service.ConstraintService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@RestController
@RequestMapping("/api/constraints")
@Tag(name = "Constraints", description = "Constraint management APIs")
public class ConstraintController {

    @Autowired
    private ConstraintService constraintService;

    @Operation(summary = "Add a constraint to an event",
            description = "Adds a constraint with a lower and upper limit for an event. The format must be YYYY-MM-DD HH:mm")
    @PostMapping("/add")
    public ResponseEntity<Map<String, String>> addConstraint(
            @Parameter(name = "eventId", description = "ID of the event", required = true)
            @RequestParam Long eventId,

            @Parameter(name = "lowerLimit", description = "Lower bound (format: YYYY-MM-DD HH:mm)", required = true)
            @RequestParam String lowerLimit,

            @Parameter(name = "upperLimit", description = "Upper bound (format: YYYY-MM-DD HH:mm)", required = true)
            @RequestParam String upperLimit,

            @Parameter(name = "username", description = "Username of the constraint submitter", required = true)
            @RequestParam String username) {

        return constraintService.addConstraint(eventId, lowerLimit, upperLimit, username);
    }
}
