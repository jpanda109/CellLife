package com.gol.game;

import com.badlogic.gdx.scenes.scene2d.Actor;
import com.badlogic.gdx.scenes.scene2d.InputEvent;
import com.badlogic.gdx.scenes.scene2d.ui.Button;
import com.badlogic.gdx.scenes.scene2d.ui.Skin;
import com.badlogic.gdx.scenes.scene2d.ui.Table;
import com.badlogic.gdx.scenes.scene2d.ui.TextButton;
import com.badlogic.gdx.scenes.scene2d.utils.ClickListener;

/**
 * Created by Jason on 2/27/2016.
 */
public class PausedStateButtons extends Table {

    public PausedStateButtons(final GameManager2D gameManager, final Basic2D gameController, Skin skin) {
        super();

        Button stepButton = new TextButton("Step", skin);
        stepButton.addListener(new ClickListener() {
            @Override
            public void clicked(InputEvent event, float x, float y) {
                gameManager.update();
            }
        });
        TextButton playButton = new TextButton("Play", skin);
        playButton.addListener(new ClickListener() {
            public void clicked(InputEvent event, float x, float y) {
                gameController.gridState = Basic2D.GridState.PLAYING;
            }
        });
        TextButton undoButton = new TextButton("Undo", skin);
        undoButton.addListener(new ClickListener() {
            public void clicked(InputEvent e, float x, float y) {
                gameManager.undo();
            }
        });

        add(playButton);
        add(stepButton);
        add(undoButton);
    }

}
