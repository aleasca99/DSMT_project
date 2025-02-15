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
import jakarta.servlet.http.HttpSession;

@WebServlet("/LoginServlet")
public class LoginServlet extends HttpServlet {
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {

        // 1. Ottieni username e password dal form
        String username = request.getParameter("username");
        String password = request.getParameter("password");

        // 2. URL dell'API REST
        String apiUrl = "http://10.2.1.11:8080/api/users/login?username=" + username + "&password=" + password;

        // 3. Creazione della connessione HTTP
        URL url = new URL(apiUrl);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("POST");
        conn.setRequestProperty("Content-Type", "application/json");
        conn.setDoOutput(true);


        // 5. Ottieni il codice di risposta del server
        int responseCode = conn.getResponseCode();

        // 6. Se il login ha successo (200 OK), reindirizza alla user_area.jsp
        if (responseCode == HttpURLConnection.HTTP_OK) {
            HttpSession session = request.getSession();
            session.setAttribute("username", username);
            response.sendRedirect("user_area.jsp");
        } else {
            response.sendRedirect("index.jsp?error=invalid_credentials");
        }
    }
}

