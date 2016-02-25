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
import com.badlogic.gdx.scenes.scene2d.utils.ChangeListener;
import com.badlogic.gdx.scenes.scene2d.utils.ClickListener;

/**
 * Created by Jason on 2/23/2016.
 */
public class Basic2D implements Screen {

    ShapeRenderer renderer;
    GameManager2D gameManager;
    float stepLength = .5f;
    float currentStepTime = 0;
    GameOfLife game;
    Stage stage;
    Table table;
    Skin skin;

    public static final String[] OPTIONS = new String[]{
            "Conway's Game of Life",
            "Gnarl"
    };

	public Basic2D (GameOfLife game) {
        renderer = new ShapeRenderer();
        gameManager = new GameManager2D();
        this.game = game;
        stage = new Stage();
        Gdx.input.setInputProcessor(stage);

        skin = new Skin(Gdx.files.internal("uiskin.json"));

        Button stepButton = new TextButton("Step", skin);
        stepButton.addListener(new ClickListener() {
            @Override
            public void clicked(InputEvent event, float x, float y) {
                gameManager.update();
            }
        });

        final SelectBox<String> selectBox = new SelectBox<String>(skin);
        final TextField cellAliveRuleField = new TextField("23", skin);
        final TextField cellDeadRuleField = new TextField("3", skin);
        selectBox.setItems(OPTIONS);
        selectBox.setSelected("Conway's Game of Life");
        selectBox.addListener(new ChangeListener() {
            @Override
            public void changed(ChangeEvent event, Actor actor) {
                switch (selectBox.getSelectedIndex()) {
                    case 0:
                        cellAliveRuleField.setText("23");
                        cellDeadRuleField.setText("3");
                        break;
                    case 1:
                        cellAliveRuleField.setText("1");
                        cellDeadRuleField.setText("1");
                        break;
                }
                gameManager.setAliveRule(cellAliveRuleField.getText());
                gameManager.setDeadRule(cellDeadRuleField.getText());
            }
        });

        Label aliveRuleLabel = new Label("AliveRule: ", skin);
        aliveRuleLabel.setColor(Color.BLACK);
        Label deadRuleLabel = new Label("DeadRule: ", skin);
        deadRuleLabel.setColor(Color.BLACK);

        cellAliveRuleField.setTextFieldListener(new TextField.TextFieldListener() {
            @Override
            public void keyTyped(TextField textField, char c) {
                gameManager.setAliveRule(textField.getText());
            }
        });
        cellAliveRuleField.setTextFieldFilter(new TextField.TextFieldFilter() {
            @Override
            public boolean acceptChar(TextField textField, char c) {
                return (c >= '0' && c <= '8');
            }
        });

        cellDeadRuleField.setTextFieldListener(new TextField.TextFieldListener() {
            @Override
            public void keyTyped(TextField textField, char c) {
                gameManager.setDeadRule(textField.getText());
            }
        });
        cellDeadRuleField.setTextFieldFilter(new TextField.TextFieldFilter() {
            @Override
            public boolean acceptChar(TextField textField, char c) {
                return (c >= '0' && c <= '8');
            }
        });

        table = new Table();
        table.setFillParent(true);
        table.left();
        table.bottom();
        stage.addActor(table);
        table.setDebug(true);
        table.add(stepButton);
        table.row();
        table.add(selectBox);
        table.row();
        table.add(aliveRuleLabel);
        table.add(cellAliveRuleField);
        table.row();
        table.add(deadRuleLabel);
        table.add(cellDeadRuleField);

        gameManager.setAliveRule(cellAliveRuleField.getText());
        gameManager.setDeadRule(cellDeadRuleField.getText());
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
        for (int i = 0; i < GameManager2D.GRID_WIDTH+1; i++) {
            renderer.line(i*10, 300, i*10, 500+300);
        }
        for (int i = 0; i < GameManager2D.GRID_HEIGHT+1; i++) {
            renderer.line(0, i*10+300, 500, i*10+300);
        }
        renderer.end();
        renderer.begin(ShapeRenderer.ShapeType.Filled);
        for (int i = 0; i < GameManager2D.GRID_WIDTH; i++) {
            for (int j = 0; j < GameManager2D.GRID_HEIGHT; j++) {
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
