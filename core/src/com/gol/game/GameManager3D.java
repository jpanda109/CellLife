package com.gol.game;

/**
 * Created by Jason on 2/23/2016.
 */
public class GameManager3D {

    public static final int GRID_X = 20;
    public static final int GRID_Y = 20;
    public static final int GRID_Z = 20;

    public boolean[][][] grid;

    public GameManager3D() {
        grid = new boolean[GRID_X][GRID_Y][GRID_Z];
        grid[1][1][1] = true;
        grid[2][1][1] = true;
        grid[3][1][1] = true;
        grid[2][2][1] = true;
        grid[2][0][1] = true;
    }

    public void update() {
        boolean[][][] nextGrid = new boolean[GRID_X][GRID_Y][GRID_Z];
        for (int i = 0; i < GRID_X; i++) {
            for (int j = 0; j < GRID_Y; j++) {
                for (int k = 0; k < GRID_Z; k++) {
                    int aliveNeighbors = getNumAliveNeighbors(i, j, k);
                    nextGrid[i][j][k] = (grid[i][j][k] && (aliveNeighbors >= 2 && aliveNeighbors <= 7))
                            || (!grid[i][j][k] && (aliveNeighbors >= 4 && aliveNeighbors <= 12));
                }
            }
        }
        grid = nextGrid;
    }

    private int getNumAliveNeighbors(int x, int y, int z) {
        int neighbors = 0;
        int x1 = (x+1)%GRID_X;
        int y1 = (y+1)%GRID_Y;
        int z1 = (z+1)%GRID_Z;
        int x2 = x-1 < 0 ? x-1+GRID_X : x-1;
        int y2 = y-1 < 0 ? y-1+GRID_Y : y-1;
        int z2 = z-1 < 0 ? z-1+GRID_X : z-1;
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                for (int k = 0; k < 3; k++) {
                    if (i == 0 && j == 0 && k == 0) {
                        continue;
                    }
                    int _x = x;
                    if (i == 1) {
                        _x = x1;
                    } else if (i == 2) {
                        _x = x2;
                    }
                    int _y = y;
                    if (j == 1) {
                        _y = y1;
                    } else if (j == 2) {
                        _y = y2;
                    }
                    int _z = z;
                    if (k == 1) {
                        _z = z1;
                    } else if (k == 2) {
                        _z = z2;
                    }
                    if (grid[_x][_y][_z]) {
                        neighbors += 1;
                    }
                }
            }
        }
        return neighbors;
    }

}
