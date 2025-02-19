package com.whenly.controller;

import com.whenly.service.UserService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.Map;

@RestController
@RequestMapping("/api/users")
@Tag(name = "Users", description = "User management APIs")
public class UserController {

    @Autowired
    private UserService userService;

    @Operation(summary = "User Sign Up", description = "Register a new user in the system")
    @PostMapping("/signup")
    public ResponseEntity<Map<String, String>> signUp(
            @Parameter(name = "username", description = "User's unique identifier", required = true)
            @RequestParam String username,

            @Parameter(name = "password", description = "User's password", required = true)
            @RequestParam String password) {

        String result = String.valueOf(userService.registerUser(username, password));
        Map<String, String> response = new HashMap<>();
        response.put("message", result);
        return ResponseEntity.ok(response);
    }

    @Operation(summary = "User Log In", description = "Authenticate an existing user")
    @PostMapping("/login")
    public ResponseEntity<Map<String, String>> logIn(
            @Parameter(name = "username", description = "User's unique identifier", required = true)
            @RequestParam String username,

            @Parameter(name = "password", description = "User's password", required = true)
            @RequestParam String password) {

        boolean success = userService.loginUser(username, password).hasBody();
        Map<String, String> response = new HashMap<>();
        response.put("message", success ? "Login successful" : "Invalid username or password");
        return ResponseEntity.ok(response);
    }
}
