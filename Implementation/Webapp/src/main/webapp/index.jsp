<%@ page import="java.net.HttpURLConnection" %>
<%@ page contentType="text/html; charset=UTF-8" pageEncoding="UTF-8" %>
<!DOCTYPE html>
<html lang="it">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Login - Whenly</title>
    <link rel="icon" type="image/png" href="img/immagine2.png">
    <link rel="stylesheet" href="css/homepage.css">
    <script type="text/javascript" src="javascript/AccessController.js"> </script>
</head>
<body>


<div class = "container">
    <div class="login-container">
        <h2>Login to Whenly</h2>

        <form id="login_form" action="LoginServlet" method="post">
            <input type="text" id="username" name="username" placeholder="Username" oninput="checkLogin()" required>
            <br>
            <br>
            <input type="password" id="password" name="password" placeholder="Password" oninput="checkLogin()" required>
            <br>
            <br>
            <button type="submit" id="login-button" name="login-button" disabled>Login</button>
        </form>
        <p>Do you haven't an account? <a href="register.jsp">Sign up</a></p>
    </div>
</div>

<%
    String username = request.getParameter("username");
    String password = request.getParameter("password");
    HttpSession session2 = request.getSession();


    HttpSession sessionObj = request.getSession();
    String error = (String) sessionObj.getAttribute("error");

    if (error != null && !error.isEmpty()) {
        sessionObj.removeAttribute("error");
%>
<script>
    alert("<%= error %>");
</script>
<%
    }
%>





</body>
</html>
