package com.gol.game;

import com.badlogic.gdx.scenes.scene2d.InputEvent;
import com.badlogic.gdx.scenes.scene2d.ui.Skin;
import com.badlogic.gdx.scenes.scene2d.ui.Table;
import com.badlogic.gdx.scenes.scene2d.ui.TextButton;
import com.badlogic.gdx.scenes.scene2d.utils.ClickListener;

/**
 * Created by Jason on 2/27/2016.
 */
public class PlayingStateButtons extends Table {

    public PlayingStateButtons(final GameManager2D gameManager, final Basic2D gameController, final ControlUI ui, Skin skin) {
        super();

        TextButton pauseButton = new TextButton("Pause", skin);
        pauseButton.addListener(new ClickListener() {
            public void clicked(InputEvent e, float x, float y) {
                gameController.gridState = Basic2D.GridState.PAUSED;
                ui.switchPlayState();
            }
        });

        add(pauseButton);
    }
}
