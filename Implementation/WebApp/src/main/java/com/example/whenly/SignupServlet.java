package com.example.whenly;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Scanner;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

@WebServlet("/SignupServlet")
public class SignupServlet extends HttpServlet {
    private static final long serialVersionUID = 1L;

    protected void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {


        String username = request.getParameter("new-username");
        String password = request.getParameter("new-password");


        String apiUrl = "http://10.2.1.11:8080/api/users/signup?username=" + username + "&password=" + password;


        URL url = new URL(apiUrl);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("POST");
        conn.setRequestProperty("Content-Type", "application/json");
        conn.setDoOutput(true);


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


        if (responseCode == HttpURLConnection.HTTP_OK) {
            response.sendRedirect("index.jsp");
        } else {
            HttpSession session = request.getSession();
            session.setAttribute("error", "error in login: username or password incorrect");

            response.sendRedirect("register.jsp");
        }

    }
}

