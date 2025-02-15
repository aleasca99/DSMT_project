package com.example.whenly;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Scanner;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@WebServlet("/SignupServlet")
public class SignupServlet extends HttpServlet {
    private static final long serialVersionUID = 1L;

    protected void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        // 1. Ottieni username e password dal form
        String username = request.getParameter("new-username");
        String password = request.getParameter("new-password");

        // 2. URL dell'API REST
        String apiUrl = "http://10.2.1.11:8080/api/users/signup?username=" + username + "&password=" + password;

        // 3. Creazione della connessione HTTP
        URL url = new URL(apiUrl);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("POST");
        conn.setRequestProperty("Content-Type", "application/json");
        conn.setDoOutput(true);

        // 4. Ottieni il codice di risposta del server
        int responseCode = conn.getResponseCode();
        System.out.println("Response Code: " + responseCode);

        Scanner scanner;
        if (responseCode == HttpURLConnection.HTTP_OK) {
            scanner = new Scanner(conn.getInputStream());
        } else {
            scanner = new Scanner(conn.getErrorStream());
        }

        StringBuilder responseMessage = new StringBuilder();
        while (scanner.hasNext()) {
            responseMessage.append(scanner.nextLine());
        }
        scanner.close();

        System.out.println("Response Message: " + responseMessage.toString());

        // 5. Se la registrazione ha successo, reindirizza
        if (responseCode == HttpURLConnection.HTTP_OK) {
            response.sendRedirect("index.jsp");
        } else {
            response.sendRedirect("register.jsp?error=" + responseMessage.toString());
        }

    }
}

