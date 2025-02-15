<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<!DOCTYPE html>
<html lang="it">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Register - Whenly</title>
  <link rel="stylesheet" href="css/homepage.css">
  <script type="text/javascript" src="javascript/AccessController.js"> </script>

</head>
<body>

<div class="container">
  <div class="register-container">
    <h2>Sign up to Whenly</h2>
    <form id="register-form" action="SignupServlet" method="post">
      <input type="text" id="new-username" name="new-username" placeholder="Username" required>
      <br>
      <br>
      <input type="password" id="new-password" name="new-password" placeholder="Password" onblur="checkPwd()" required>
      <span id="pwd-error" style="color:red; display: none;"> Passwords must be containt a number and a upper case letter </span>
      <br>
      <br>
      <input type="password" id="repeat-password" name="repeat-password" placeholder="Repeat Password" onblur="matchPwd()"  required>
      <span id="error-Msg" style="color:red; display: none;"> Passwords don't match </span>
      <br>
      <br>
      <button type="submit" id="register-button" name="register-button" disabled>Register</button>
    </form>
  </div>
</div>

</body>
</html>
