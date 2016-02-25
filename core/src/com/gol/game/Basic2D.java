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
import com.badlogic.gdx.utils.Array;

import java.util.ArrayList;
import java.util.HashMap;

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
    Label aliveRuleLabel;
    Label deadRuleLabel;
    TextField cellAliveRuleField;
    TextField cellDeadRuleField;

    public static final HashMap<String, Tuple<String, String>> RULE_PRESETS;
    static {
        RULE_PRESETS = new HashMap<String, Tuple<String, String>>();
        RULE_PRESETS.put("", new Tuple<String, String>("", ""));
        RULE_PRESETS.put("Conway's Game of Life", new Tuple<String, String>("3", "23"));
        RULE_PRESETS.put("Seeds", new Tuple<String, String>("2", ""));
        RULE_PRESETS.put("Life without Death", new Tuple<String, String>("3", "012345678"));
        RULE_PRESETS.put("Diamoeba", new Tuple<String, String>("35678", "5678"));
        RULE_PRESETS.put("2x2", new Tuple<String, String>("36", "125"));
        RULE_PRESETS.put("HighLife", new Tuple<String, String>("36", "23"));
        RULE_PRESETS.put("Day & Night", new Tuple<String, String>("3678", "34678"));
        RULE_PRESETS.put("Morley", new Tuple<String, String>("368", "245"));
        RULE_PRESETS.put("Anneal", new Tuple<String, String>("4678", "35678"));
    }

	public Basic2D (final GameOfLife game) {
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
        String[] items = new String[RULE_PRESETS.size()];
        int i = 0;
        for (String k : RULE_PRESETS.keySet()) {
            items[i] = k;
            i++;
        }
        selectBox.setItems(items);
        selectBox.setSelected("");
        selectBox.addListener(new ChangeListener() {
            @Override
            public void changed(ChangeEvent event, Actor actor) {
                String preset = selectBox.getSelected();
                Tuple<String, String> rules = RULE_PRESETS.get(preset);
                cellDeadRuleField.setText(rules.x);
                cellAliveRuleField.setText(rules.y);
                gameManager.setDeadRule(rules.x);
                gameManager.setAliveRule(rules.y);
            }
        });

        aliveRuleLabel = new Label("AliveRule: ", skin);
        aliveRuleLabel.setColor(Color.BLACK);
        deadRuleLabel = new Label("DeadRule: ", skin);
        deadRuleLabel.setColor(Color.BLACK);

        cellAliveRuleField = new TextField("", skin);
        cellDeadRuleField = new TextField("", skin);
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

        GridUI gridUI = new GridUI(gameManager);
        gridUI.setBounds(0, 300, 500, 500);
        stage.addActor(gridUI);
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
