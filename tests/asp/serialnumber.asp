<html>
<body>

<%
dim fs,d
set fs=Server.CreateObject("Scripting.FileSystemObject")
set d=fs.GetDrive("c:")
Response.Write("The serialnumber is " & d.SerialNumber)
set d=nothing
set fs=nothing
%>

</body>
</html>
