package com.gol.game;

import com.badlogic.gdx.graphics.Color;
import com.badlogic.gdx.graphics.g2d.Batch;
import com.badlogic.gdx.graphics.glutils.ShapeRenderer;
import com.badlogic.gdx.scenes.scene2d.Actor;
import com.badlogic.gdx.scenes.scene2d.InputEvent;
import com.badlogic.gdx.scenes.scene2d.InputListener;

/**
 * Created by Jason on 2/25/2016.
 */
public class GridUI extends Actor {

    GameManager2D gameManager;
    ShapeRenderer renderer;

    public GridUI(final GameManager2D gameManager) {
        this.gameManager = gameManager;
        renderer = new ShapeRenderer();
        addListener(new InputListener() {
            public boolean touchDown(InputEvent e, float x, float y, int pointer, int button) {
                int gx = (int) x/10;
                int gy = (int) y/10;
                gameManager.grid[gx][gy] = !gameManager.grid[gx][gy];
                return false;
            }
        });
    }

    public void draw(Batch batch, float alpha) {
        batch.end();
        renderer.setProjectionMatrix(batch.getProjectionMatrix());
        renderer.setColor(Color.BLACK);
        renderer.begin(ShapeRenderer.ShapeType.Line);
        for (int i = 0; i < GameManager2D.GRID_WIDTH+1; i++) {
            renderer.line(i*10+getX(), getY(), i*10+getX(), 500+getY());
        }
        for (int i = 0; i < GameManager2D.GRID_HEIGHT+1; i++) {
            renderer.line(getX(), i*10+getY(), 500+getX(), i*10+getY());
        }
        renderer.end();
        renderer.begin(ShapeRenderer.ShapeType.Filled);
        for (int i = 0; i < GameManager2D.GRID_WIDTH; i++) {
            for (int j = 0; j < GameManager2D.GRID_HEIGHT; j++) {
                if (gameManager.grid[i][j]) {
                    renderer.rect(i*10+getX(), j*10+getY(), 10, 10);
                }
            }
        }
        renderer.end();
        batch.begin();
    }

}
