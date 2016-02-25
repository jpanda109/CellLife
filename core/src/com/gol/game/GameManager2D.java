package com.gol.game;

import com.badlogic.gdx.scenes.scene2d.Actor;
import com.badlogic.gdx.scenes.scene2d.InputEvent;
import com.badlogic.gdx.scenes.scene2d.InputListener;

/**
 * Created by Jason on 2/23/2016.
 */
public class GameManager2D {

    public static final int GRID_WIDTH = 50;
    public static final int GRID_HEIGHT = 50;
    public boolean[][] grid = new boolean[GRID_WIDTH][GRID_HEIGHT];
    public boolean[] aliveRule = new boolean[9];
    public boolean[] deadRule = new boolean[9];

    public GameManager2D() {
        grid[1][1] = true;
        grid[2][1] = true;
        grid[3][1] = true;
    }

    public void setAliveRule(String ruleString) {
        setRule(aliveRule, ruleString);
    }

    public void setDeadRule(String ruleString) {
        setRule(deadRule, ruleString);
    }

    private void setRule(boolean[] rule, String ruleString) {
        for (int i = 0; i < rule.length; i++) {
            rule[i] = false;
        }
        for (char c : ruleString.toCharArray()) {
            int i = Character.getNumericValue(c);
            rule[i] = true;
        }
    }

    public void update() {
        boolean[][] nextGrid = new boolean[GRID_WIDTH][GRID_HEIGHT];
        for (int i = 0; i < GRID_WIDTH; i++) {
            for (int j = 0; j < GRID_HEIGHT; j++) {
                int aliveNeighbors = getNumAliveNeighbors(i, j);
                nextGrid[i][j] = (grid[i][j] && aliveRule[aliveNeighbors])
                                    || (!grid[i][j] && deadRule[aliveNeighbors]);
            }
        }
        grid = nextGrid;
    }

    public int getNumAliveNeighbors(int x, int y) {
        boolean[] neighbors = new boolean[8];
        int x1 = (x+1)%GRID_WIDTH;
        int y1 = (y+1)%GRID_HEIGHT;
        int x2 = x-1 < 0 ? x-1+GRID_WIDTH : x-1;
        int y2 = y-1 < 0 ? y-1+GRID_HEIGHT : y-1;
        neighbors[0] = grid[x1][y1];
        neighbors[1] = grid[x1][y2];
        neighbors[2] = grid[x1][y];
        neighbors[3] = grid[x2][y1];
        neighbors[4] = grid[x2][y2];
        neighbors[5] = grid[x2][y];
        neighbors[6] = grid[x][y1];
        neighbors[7] = grid[x][y2];
        int count = 0;
        for (boolean n : neighbors) {
            if (n) {
                count += 1;
            }
        }
        return count;
    }

}
