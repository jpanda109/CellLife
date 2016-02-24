package com.gol.game;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Screen;
import com.badlogic.gdx.graphics.Color;
import com.badlogic.gdx.graphics.GL20;
import com.badlogic.gdx.graphics.glutils.ShapeRenderer;
import com.badlogic.gdx.scenes.scene2d.Actor;
import com.badlogic.gdx.scenes.scene2d.InputEvent;
import com.badlogic.gdx.scenes.scene2d.Stage;
import com.badlogic.gdx.scenes.scene2d.ui.*;
import com.badlogic.gdx.scenes.scene2d.utils.ClickListener;
import com.sun.xml.internal.bind.annotation.OverrideAnnotationOf;

/**
 * Created by Jason on 2/23/2016.
 */
public class Basic2D implements Screen {

    ShapeRenderer renderer;
    GameManager gameManager;
    float stepLength = .5f;
    float currentStepTime = 0;
    GameOfLife game;
    Stage stage;
    Table table;
    Skin skin;

	public Basic2D (GameOfLife game) {
        renderer = new ShapeRenderer();
        gameManager = new GameManager();
        this.game = game;
        stage = new Stage();
        Gdx.input.setInputProcessor(stage);
        table = new Table();
        stage.addActor(table);
        table.setDebug(true);
        skin = new Skin(Gdx.files.internal("uiskin.json"));

        Button stepButton = new TextButton("Step", skin);
        stepButton.addListener(new ClickListener() {
            @Override
            public void clicked(InputEvent event, float x, float y) {
                gameManager.update();
            }
        });
        table.add(stepButton);
        table.row();

        SelectBox<String> selectBox = new SelectBox<String>(skin);
        selectBox.setItems("Conway's Game of Life", "Brian's Brain");
        selectBox.setSelected("Conway's Game of Life");
        table.add(selectBox).left().bottom().padLeft(300).padBottom(500).minWidth(200);
    }

	@Override
	public void render(float delta) {
        update(delta);
        draw();
	}

    private void update(float delta) {
//        currentStepTime += delta;
//        if (currentStepTime > stepLength) {
//            currentStepTime -= stepLength;
//            gameManager.update();
//        }
        stage.act(delta);
    }

    private void draw() {
        Gdx.gl.glClearColor(1, 1, 1, 1);
		Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT);
        renderer.setColor(Color.BLACK);
        renderer.begin(ShapeRenderer.ShapeType.Line);
        for (int i = 0; i < GameManager.GRID_WIDTH+1; i++) {
            renderer.line(i*10, 300, i*10, 500+300);
        }
        for (int i = 0; i < GameManager.GRID_HEIGHT+1; i++) {
            renderer.line(0, i*10+300, 500, i*10+300);
        }
        renderer.end();
        renderer.begin(ShapeRenderer.ShapeType.Filled);
        for (int i = 0; i < GameManager.GRID_WIDTH; i++) {
            for (int j = 0; j < GameManager.GRID_HEIGHT; j++) {
                if (gameManager.grid[i][j]) {
                    renderer.rect(i*10, j*10+300, 10, 10);
                }
            }
        }
        renderer.end();
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
        renderer.dispose();
        skin.dispose();
        stage.dispose();
    }
}
