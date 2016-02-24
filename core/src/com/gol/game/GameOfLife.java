package com.gol.game;

import com.badlogic.gdx.Game;
import com.badlogic.gdx.Screen;

public class GameOfLife extends Game {

    @Override
    public void create() {
        setScreen(new Basic2D(this));
    }

    public void replaceScreen(Screen screen) {
        Screen old = getScreen();
        setScreen(screen);
        old.dispose();
    }
}
