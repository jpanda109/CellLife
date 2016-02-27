package com.gol.game;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Screen;
import com.badlogic.gdx.graphics.Color;
import com.badlogic.gdx.graphics.GL20;
import com.badlogic.gdx.scenes.scene2d.Actor;
import com.badlogic.gdx.scenes.scene2d.Stage;
import com.badlogic.gdx.scenes.scene2d.ui.*;
import com.badlogic.gdx.scenes.scene2d.utils.ChangeListener;

import java.util.HashMap;

/**
 * Created by Jason on 2/23/2016.
 */
public class Basic2D implements Screen {

    GameManager2D gameManager;
    float stepLength = .5f;
    float currentStepTime = 0;
    GameOfLife game;
    Stage stage;
    public enum GridState {PLAYING, PAUSED};
    GridState gridState = GridState.PAUSED;


	public Basic2D (final GameOfLife game) {
        gameManager = new GameManager2D();
        this.game = game;
        stage = new Stage();
        Gdx.input.setInputProcessor(stage);


        GridUI gridUI = new GridUI(gameManager);
        gridUI.setBounds(0, 300, 500, 500);
        stage.addActor(gridUI);
        stage.addActor(new ControlUI(this, gameManager));
    }

	@Override
	public void render(float delta) {
        update(delta);
        draw();
	}

    private void update(float delta) {
        switch (gridState) {
            case PAUSED:
                break;
            case PLAYING:
                currentStepTime += delta;
                if (currentStepTime > stepLength) {
                    currentStepTime -= stepLength;
                    gameManager.update();
                }
        }
        stage.act(delta);
    }

    private void draw() {
        Gdx.gl.glClearColor(1, 1, 1, 1);
		Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT);
        stage.draw();
    }

    @Override
    public void show() {

    }

    @Override
    public void resize(int width, int height) {

    }

    @Override
    public void pause() {

    }

    @Override
    public void resume() {

    }

    @Override
    public void hide() {

    }

    @Override
    public void dispose() {
        stage.dispose();
    }
}
