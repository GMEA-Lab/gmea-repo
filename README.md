# gmea-repo
Store code here to track your changes and development. Collaborate on routines. Share your code with your colleagues..


Step-by-step guide to use this (or any repository).
1. Make a github account on github.com [tell me when you do this, I can add you to the group]
2. Download Git Bash https://git-scm.com/downloads
3. Open Git Bash, or any command window.
4. Navigate to the where you will want the repository to be. I suggest your user directory; type "cd ~".
5. type "git clone https://github.com/GMEA-Lab/gmea-repo.git" ---> This command created a new folder called "gmea-repo", it is an exact CLONE of the one that is online available though the github page.
6. Now you should probably set up your username and password: https://help.github.com/en/github/using-git/setting-your-username-in-git 
7. Contributing to the repository (the new folder we all share)
8. Add a file to your "gmea-repo" folder. For this example I will call it "WadesEVrecipe.docx".
9. Type "git add WadesEVrecipe.docx" ---> This command makes git add it to the list of files it follows and cares about. It will from now on keep tabs on WadesEVrecipe.docx, because I told git to ADD it to it's list of things to care about.
10. Type "git commit WadesEVrecipe.docx". Add a message like " Add EV cheatsheet". Save and close. ---> You are now writting the journal entry that will be attached to this file for the last things you did to it. All we did was add it to the repository. I am writting a note that says that. If I changed the files and updated one of the steps to improve a filter I might write something like "Improved absorption filter".
11. The file WadesEVrecipe.docx and all the other changes I may have made now exists on my computers version (local) of the repository but I want to PUSH it to the world wide web so that all my collegues can see it on the github official version (remote).
12. Type "git push". You will need to enter your github usename/password. (there are ways to fix that, I don't like the administrative stuf, but the internet should know.)


Everytime you want to use to folder.
1. FIRST THING: git pull ---> You can get into some nasty situations if you start trying to work before doing GIT PULL. Everything you want to do something in on about near this folder start with GIT PULL. It will update you local version to match the remote verison that everyone else has been adding to and working on. It gives you the most updated official version.
2. Do you things, add files, change files ++. For everything thing you do; do a quick "git commit filename". It'll help you and your collaborators keep tab on what you're doing. Just quick little comments don't fret.
3. GIT PUSH. ----> When you're done do "git push" so that everyone else can reap the rewards of your hard and dilligent work. If you don't push your changes to the remote version no one can access what you did. If you are working on the same file as some one else PUSH often, PULL often. *muscle emoji*

Let me (or https://help.github.com/en/github) know if you have questions.
Muriel

