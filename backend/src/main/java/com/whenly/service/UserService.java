package com.whenly.service;

import com.whenly.model.User;
import com.whenly.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
public class UserService {

    @Autowired
    private UserRepository userRepository;

    private final BCryptPasswordEncoder passwordEncoder = new BCryptPasswordEncoder();

    public String registerUser(String username, String password) {
        // Controlla se l'utente esiste già
        if (userRepository.findByUsername(username).isPresent()) {
            return "User already exists";
        }

        // Hasher la password
        String hashedPassword = passwordEncoder.encode(password);
        User newUser = new User(username, hashedPassword);
        userRepository.save(newUser);
        return "User registered successfully";
    }

    public boolean loginUser(String username, String password) {
        Optional<User> userOpt = userRepository.findByUsername(username);
        if (userOpt.isEmpty()) {
            return false; // Utente non trovato
        }

        // Controlla se la password inserita corrisponde
        return passwordEncoder.matches(password, userOpt.get().getPassword());
    }
}
