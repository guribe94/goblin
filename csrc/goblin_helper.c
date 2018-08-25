#include <stdio.h>
#include <sys/select.h>
#include <unistd.h>
#include <termios.h>
#include <stdlib.h>

#define NB_DISABLE 0
#define NB_ENABLE 1

int _clearScreen();
int kbhit();
void nonblock(int);



int kbhit()
{
    struct timeval tv;
    fd_set fds;
    tv.tv_sec = 0;
    tv.tv_usec = 0;
    FD_ZERO(&fds);
    FD_SET(STDIN_FILENO, &fds); //STDIN_FILENO is 0
    select(STDIN_FILENO+1, &fds, NULL, NULL, &tv);
    return FD_ISSET(STDIN_FILENO, &fds);
}

void nonblock(int state)
{
    struct termios ttystate;

    //get the terminal state
    tcgetattr(STDIN_FILENO, &ttystate);

    if (state==NB_ENABLE)
    {
        //turn off canonical mode
        ttystate.c_lflag &= ~ICANON;
        //minimum of number input read.
        ttystate.c_cc[VMIN] = 1;
        //Disable echo bit
        ttystate.c_lflag &= ~ECHO;
    }
    else if (state==NB_DISABLE)
    {
        //turn on canonical mode
        ttystate.c_lflag |= ICANON;
        //Disable echo bit
        ttystate.c_lflag |= ECHO;
    }
    //set the terminal attributes.
    tcsetattr(STDIN_FILENO, TCSANOW, &ttystate);
}


char getKey(){

    char c;
    int i=0;

    nonblock(NB_ENABLE);
    while(!i)
    {
        usleep(1);
        i=kbhit();
        //Do this if there is something in the buffer
        if (i!=0)
        {
            c=fgetc(stdin);
            if (c){
                i=1;
            }
            else{
                i=0;
            }
        }

    }
        nonblock(NB_DISABLE);
        return c;
}


int _clearScreen(){
  system("clear");
  return 0;
}
