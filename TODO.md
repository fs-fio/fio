1. Does it make sense to have both, bothCont, and successCont and failureCont? Can't we just use bothCont or the two other? Why should we have 3?

2. Can we make MailBoxQueue constructor private?

3. Should we rewrite cancelToken (everywhere in the project) to CancellationToken? What is more .NET idiomatic?

4. In the Channel type, we now have Send and Offer. I want to keep it Read and Write. Can we name it something else, perhaps like WriteUnit? Or something? What would ZIO do?

5. Is it really wise to introduce 3 new FIO DUs?
