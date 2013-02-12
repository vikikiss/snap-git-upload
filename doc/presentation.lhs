%% -*- latex -*-
\documentclass{beamer}
%% \usetheme{Boadilla}
%% \usecolortheme{beaver}
%% \usetheme{Montpellier}
%% \usetheme{Singapore}
%% \usecolortheme{beaver}
\usetheme{Madrid}
\usecolortheme{whale}
\setbeamertemplate{footline}[page number]{}
\usepackage[utf8]{inputenc}

%include polycode.fmt
%% %include mystyle.fmt
%% \arrayhs
\newcommand{\backtick}[1]{\;\Varid{\text{\`{}}\!#1\!\text{\`{}}}\;}
        
\begin{document}

\title{Prototype of a web application for uploading, compiling and
  committing a Java file to Git repository}
\subtitle{using Haskell's Snap and HSH libraries}
\author{Viktoria Kiss}
\date{15\textsuperscript{th} Feb 2013}

\maketitle

\begin{frame}{Task}
  \begin{itemize}
    \item Users can upload a Java file to the server on a web interface
    \item After uploading the program compiles the file
    \item If the compilation is successful, then it commits \& pushes to a Git
      repository
    \item If any error occurs during the whole process -- during
      uploading, compilation, committing -- the repo is reset to its
      last commit
    \item It prints a report to the console about the result (could send an email)
  \end{itemize}
\end{frame}

\begin{frame}{Technologies Used}
  \begin{itemize}
    \item \emph{Haskell}
    \item \emph{Snap} -- web combinator library
    \item \emph{HSH} -- for running shell commands
    \item \emph{EitherT} -- error propagation
  \end{itemize}  
\end{frame}

\begin{frame}{Demo}
\end{frame}

\begin{frame}{Architecture}
  \begin{block}{}
  \begin{spec}
main = do    
    chan <- newChan
    forkIO $ forever $ do
        processUpload =<< readChan chan
    quickHttpServe $ site chan    
  \end{spec}
  \end{block}
 
  \begin{itemize}
  \item In the main function I use a channel to communicate between
    web frontend and processing backend. Because users can upload
    files in the same time, the channel makes the proccessing
    sequential. I use a single thread for the processing.
  \item Snap helps web part of the application (including routes, HTML
    files, save the uploaded file to a temporary location)
  \item HSH helps to run shell commands for compiling the Java file,
    commit and push it to Git repository
\end{itemize}
\end{frame}

\begin{frame}{Front-end code for uploads}
  \begin{block}{}
    %% \footnotesize
    \begin{spec}
      bs <- BS.readFile tmpFileName
      (filePath, h) <-  openBinaryTempFile
                          "files/queue"
                          "upload.java"
      BS.hPut h bs
      hClose h

      let job = Job  { username  =  username
                     , filePath  =  filePath
                     , fileName  =  fileName
                     }
      writeChan chan job
    \end{spec}
  \end{block}
\end{frame}

\begin{frame}{Backend processing of uploads}
  \begin{block}{}
  \begin{spec}
processUpload :: Job -> IO ()
processUpload job = do
    result <- runEitherT $ do
        moveToDestination job
        compile job
        git job
    cleanup
    
    case result of
        Left err -> do
            putStrLn "Error!"
            BS.putStrLn err
        Right () -> putStrLn "success!"
  \end{spec}
  \end{block}
\end{frame}

\begin{frame}{Some interesting details}
  \begin{itemize}
    \item Uploading a file requires authentication
      \begin{spec}
        getUsername =  forceJust maybeLoginCookie
                         <|> redirect "/login"
      \end{spec}
    \item Usage:
      \begin{spec}
        ("doUpload", getUsername >>= upload chan)
      \end{spec}
    \item HSH patch -- HSH returns only the standard output, but for
      the report we need the standard error as well; Patch sent to upstream
    \item Login token -- I used SHA1 for hashing the username, ip
      address and a secret key into a cookie
  \end{itemize}
\end{frame}

\begin{frame}{Future work}
  \begin{itemize}
    \item Heist for dynamic page templates
    \item Listing page for queued jobs
    \item Login redirect
  \end{itemize}
\end{frame}

\begin{frame}{}
  \Huge
  \begin{center}
    Thank you!\\[2em]
  \end{center}
\end{frame}

\end{document}
