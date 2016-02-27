package com.gol.game;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.graphics.Color;
import com.badlogic.gdx.scenes.scene2d.Actor;
import com.badlogic.gdx.scenes.scene2d.ui.*;
import com.badlogic.gdx.scenes.scene2d.utils.ChangeListener;

import java.util.HashMap;

/**
 * Created by Jason on 2/27/2016.
 */
public class ControlUI extends Table {

    PlayingStateButtons playingStateButtons;
    PausedStateButtons pausedStateButtons;
    Basic2D gameController;
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

    public ControlUI(Basic2D gameController, final GameManager2D gameManager) {
        this.gameController = gameController;
        Skin skin = new Skin(Gdx.files.internal("uiskin.json"));
        final TextField cellAliveRuleField = new TextField("", skin);
        final TextField cellDeadRuleField = new TextField("", skin);
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

        pausedStateButtons = new PausedStateButtons(gameManager, gameController, this, skin);
        playingStateButtons = new PlayingStateButtons(gameManager, gameController, this, skin);

        Label presetsLabel = new Label("Presets: ", skin);
        presetsLabel.setColor(Color.BLACK);

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
            public void changed(ChangeListener.ChangeEvent event, Actor actor) {
                String preset = selectBox.getSelected();
                Tuple<String, String> rules = RULE_PRESETS.get(preset);
                cellDeadRuleField.setText(rules.x);
                cellAliveRuleField.setText(rules.y);
                gameManager.setDeadRule(rules.x);
                gameManager.setAliveRule(rules.y);
            }
        });

        Label aliveRuleLabel = new Label("AliveRule: ", skin);
        aliveRuleLabel.setColor(Color.BLACK);
        Label deadRuleLabel = new Label("DeadRule: ", skin);
        deadRuleLabel.setColor(Color.BLACK);


        gameManager.setAliveRule(cellAliveRuleField.getText());
        gameManager.setDeadRule(cellDeadRuleField.getText());

        setFillParent(true);
        left();
        bottom();
        setDebug(true);
        add(pausedStateButtons);
        row();
        add(presetsLabel);
        add(selectBox);
        row();
        add(aliveRuleLabel);
        add(cellAliveRuleField);
        row();
        add(deadRuleLabel);
        add(cellDeadRuleField);

    }

    public void switchPlayState() {
        Cell cell = getCell(pausedStateButtons);
        if (cell != null) {
            cell.setActor(playingStateButtons);
        } else {
            getCell(playingStateButtons).setActor(pausedStateButtons);
        }
    }

}
