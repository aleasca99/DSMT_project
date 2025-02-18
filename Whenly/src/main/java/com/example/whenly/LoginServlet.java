package com.example.whenly;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
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

        // Get username e password from form
        String username = request.getParameter("username");
        String password = request.getParameter("password");

        // API REST URL
        String apiUrl = "http://10.2.1.11:8080/api/users/login?username=" + username + "&password=" + password;

        // Creation HTTP connection
        URL url = new URL(apiUrl);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("POST");
        conn.setRequestProperty("Content-Type", "application/json");
        conn.setDoOutput(true);


        // Get response code from server
        int responseCode = conn.getResponseCode();

        HttpSession session = request.getSession();
        // if user login successfully (200 OK), redirect to user_area.jsp
        if (responseCode == HttpURLConnection.HTTP_OK) {
            session.setAttribute("username", username);
            response.sendRedirect("user_area.jsp");
        } else {
            session.setAttribute("error", "invalid credentials");
            response.sendRedirect("index.jsp");
        }
    }
}

