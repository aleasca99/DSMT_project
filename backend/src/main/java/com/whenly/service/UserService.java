package com.whenly.service;

import com.whenly.model.User;
import com.whenly.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;

import java.util.Optional;

@Service
public class UserService {

    // Repository to interact with the User data in the database
    @Autowired
    private UserRepository userRepository;

    // BCryptPasswordEncoder is used to securely hash and verify passwords
    private final BCryptPasswordEncoder passwordEncoder = new BCryptPasswordEncoder();

    /**
     * Registers a new user with the provided username and password.
     * The password is hashed using BCrypt before being stored.
     *
     * @param username the username to register.
     * @param password the plaintext password to hash and register.
     * @return a ResponseEntity with a success message upon registration.
     * @throws ResponseStatusException if the user already exists (HTTP 409 Conflict).
     */
    public ResponseEntity<String> registerUser(String username, String password) {
        // Check if a user with the given username already exists
        if (userRepository.findByUsername(username).isPresent()) {
            throw new ResponseStatusException(HttpStatus.CONFLICT, "User already exists");
        }

        // Hash the plaintext password using BCrypt
        String hashedPassword = passwordEncoder.encode(password);
        // Create a new User object with the provided username and hashed password
        User newUser = new User(username, hashedPassword);
        // Save the new user to the database
        userRepository.save(newUser);

        // Return a success response
        return ResponseEntity.ok("User registered successfully");
    }

    /**
     * Logs in a user by verifying the provided username and password.
     *
     * @param username the username for login.
     * @param password the plaintext password to verify.
     * @return a ResponseEntity with a success message if credentials are valid.
     * @throws ResponseStatusException if the credentials are invalid (HTTP 401 Unauthorized).
     */
    public ResponseEntity<String> loginUser(String username, String password) {
        // Retrieve the user by username from the repository
        Optional<User> userOpt = userRepository.findByUsername(username);

        // Check if the user exists and if the password matches the stored hash
        if (userOpt.isPresent() && passwordEncoder.matches(password, userOpt.get().getPassword())) {
            return ResponseEntity.ok("Login successful");
        }

        // If login fails, throw an exception indicating unauthorized access
        throw new ResponseStatusException(HttpStatus.UNAUTHORIZED, "Invalid username or password");
    }
}
