typedef struct _win_st WINDOW;
typedef unsigned int chtype;

WINDOW *initscr   (void);
WINDOW *newwin    (int, int, int, int);
void    endwin    (void);
void    refresh   (void);
void    wrefresh  (WINDOW *);
void    addstr (const char *);
int     mvwaddch  (WINDOW *, int, int, const chtype);
void    mvwaddstr (WINDOW *, int, int, char *);
void    box (WINDOW *, chtype, chtype);
int     cbreak (void);
