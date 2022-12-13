# Developer Guide of RACKET SANKE GAME

#### The Data Structure

* World State: (w_state)
    * Snake State (s_state)
    * A list of Food State (f_state)

* Snake State: (s_state)
    * a list of positions of each part of snake (pos)
    * snake move direction (dir) 
    * size of snake part (sp)
    * color of snake (col)

* Food State: (f_state)
    * position of food (pos)
    * color of food (col)
    * type of game (ty)

#### The Design of the RACKET SNAKE GAME

The whole game is mainly based on the usage of racket/base, 2htdp/image(for render page) and 2htdp/universe(for tick world state). There are two universes prepared in the game. One is for the game page and another one is for the start game page. The start game page universe will trigger the game page universe with different parameters, so that we get diiferent game page universe started.

* ##### The start game page universe (start-game)
    The function *begnning* is used for drawing the start game page and *handle-mouse* is used to make sure that event happened on the buttons we created could be correctly listened.

* ##### The game page (play-easy play-medium play-hard)
    The most fundemental logic of the snake game is we take a world state which contains a snake state and list of food state and then we update it based on different situations.

#### Top Level Functions for Snake Game

* ##### Move Function (move_right, move_left, move_up,move_down)
    *move* series function will update the snake state position list base on the following rules: the last elemnt in position list will be conside as the head of the snake, and then it will be update based on the snake direction and the snake part size to make that the snake head will move one step to the snake direction, and the coordinates of the remaining parts of the snake will become the coordinates of the part they are next to.

* ##### Eat Function (eat-normal, eat-poison, eat-double)
    *eat* series function will add a new part in the snake position list. based on which direction the snake eat the food(this is why we need to use *right_to_head* *left_to_head* *up_to_head* *down_to_head* series functions). And by set different parameters for *update_food* and *_to_head* functions we could set different *eat* series functions. (like *eat-normal eat-poison eat-double*)

    > *det?* and *eat?* functions are used for judege if the snake eat the food

* ##### Draw Function (draw_snake, draw_food, draw_score)
    *draw* series function will generate the page based on the world state.
    By change the local definiation in *draw_snake* and *draw_food*, we could get different snake and food images.

* ##### Handle-key 
    *handle-key* function to set how to operate the game.

* ##### Tick
    *tick* function is used to listen to all events that occur during the game.

    


