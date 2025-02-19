

function checkPwd()
{

    let password = document.getElementById("new-password");
    let error_msg = document.getElementById("pwd-error");
    let correct = true;


    let lowerCase = /[a-z]/g;
    if(!password.value.match(lowerCase))
    {
        correct = false;
    }

    let upperCase = /[A-Z]/g;
    if(!password.value.match(upperCase))
    {
        correct = false;
    }

    let numbers = /[0-9]/g;
    if(!password.value.match(numbers))
    {
        correct = false;
    }

    if(!(password.value.length >= 8))
    {
        correct = false;
    }

    if(!correct)
    {
        error_msg.style.display = "block";
        password.style.borderColor = "red";
    }
    else
    {
        error_msg.style.display = "none";
        password.style.borderColor = "black";
    }


}

function matchPwd()
{
    let password = document.getElementById("new-password");
    let repeatedPwd = document.getElementById("repeat-password");
    let error_msg = document.getElementById("error-Msg");
    let button = document.getElementById("register-button");
    let correct = true;

    if(password.value === repeatedPwd.value)
        correct = true;
    else
        correct = false;

    if(!correct)
    {
        repeatedPwd.style.borderColor = "red";
        error_msg.style.display = "block";
    }
    else
    {
        repeatedPwd.style.borderColor = "black";
        error_msg.style.display = "none";
        button.disabled = false;
    }


}

function checkLogin()
{
    let username = document.getElementById("username");
    let password = document.getElementById("password");
    let button = document.getElementById("login-button");

    if(username.value !== '' && password.value !== '')
    {
        button.disabled = false;
    }

}

