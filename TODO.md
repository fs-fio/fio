

1. Hmm... What to name the libraries? I am not sure.




There is a fatal error during hsutdown event when a fiber gets interrputed.

fatal: Unable to cast object of type 'FiberInterruptedException' to type 'FSharp.FIO.Sockets.SocketError'.



1. Drain issue
    - Is it really necessary for us to have to drain those channels?
    - If it is necessary, we need to make sure that only one Run instance method can be active before a fiber has completed.

2. App user experience and architecture?

3. Test everything

4. Experimental stuff

5. EF Core?
