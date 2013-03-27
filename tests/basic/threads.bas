SuperStrict

' Threading tutorial 1:
' A basic loading thread


' a threadable function
' threadable functions must return an Object and take 1 object as input, they don't need to be used
Function loadResources:Object(in:Object)
        Print "Starting a child thread..." 
        For Local counter:Int = 0 Until 20 ' just a loop to make stuff happen
                Print "Pretending to load resource " + counter
                Delay(300) ' just to make this take some time like loading a real resource would
        Next
        Print "Child thread complete."
End Function



'####### Main code starts here

' Create a thread with loadResources() and Null as it's input object value
Local loadingThread:TThread = CreateThread(loadResources, Null)

Print "Starting the main loop..."
While(ThreadRunning(loadingThread)) ' as long as that child thread is still running...
        Print "Waiting on our resources..."
        Delay(100) ' we could do whatever we want here...
Wend
Print "Main loop complete."
