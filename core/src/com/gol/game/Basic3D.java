package com.gol.game;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Screen;
import com.badlogic.gdx.graphics.*;
import com.badlogic.gdx.graphics.g3d.*;
import com.badlogic.gdx.graphics.g3d.attributes.ColorAttribute;
import com.badlogic.gdx.graphics.g3d.environment.DirectionalLight;
import com.badlogic.gdx.graphics.g3d.utils.CameraInputController;
import com.badlogic.gdx.graphics.g3d.utils.ModelBuilder;
import com.badlogic.gdx.graphics.glutils.ShapeRenderer;
import com.badlogic.gdx.utils.Array;

/**
 * Created by Jason on 2/23/2016.
 */
public class Basic3D implements Screen {

    GameOfLife game;
    PerspectiveCamera cam;
    ModelBatch batch;
    Model model;
    Array<ModelInstance> modelInstances;
    Environment environment;
    ShapeRenderer renderer;
    CameraInputController camController;
    GameManager3D gameManager;
    float stepLength;
    float currentStepTime;

    public Basic3D(GameOfLife game) {
        this.game = game;
        renderer = new ShapeRenderer();
        cam = new PerspectiveCamera(67, 500, 500);
        cam.position.set(300, 300, 300);
        cam.lookAt(0, 0, 0);
        cam.near = 1f;
        cam.far = 500f;
        cam.update();
        camController = new CameraInputController(cam);
        Gdx.input.setInputProcessor(camController);

        ModelBuilder modelBuilder = new ModelBuilder();
        model = modelBuilder.createBox(10, 10, 10,
                new Material(ColorAttribute.createDiffuse(Color.GREEN)),
                VertexAttributes.Usage.Position | VertexAttributes.Usage.Normal);
        batch = new ModelBatch();
        environment = new Environment();
        environment.set(new ColorAttribute(ColorAttribute.AmbientLight, 0.4f, 0.4f, 0.4f, 1f));
        environment.add(new DirectionalLight().set(0.8f, 0.8f, 0.8f, -1f, -0.8f, -02.f));
        gameManager = new GameManager3D();
        modelInstances = new Array<ModelInstance>();
        updateInstances();
        stepLength = 1;
        currentStepTime = 0;
    }

    @Override
    public void render(float delta) {
        update(delta);
        draw();
    }

    private void update(float delta) {
        currentStepTime += delta;
        if (currentStepTime > stepLength) {
            currentStepTime -= stepLength;
            gameManager.update();
            updateInstances();
        }
    }

    private void draw() {
        camController.update();
        Gdx.gl.glViewport(0, 300, 500, 500);
        Gdx.gl.glClearColor(0, 0, 0, 1);
        Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT | GL20.GL_DEPTH_BUFFER_BIT);
        batch.begin(cam);
        for (ModelInstance instance : modelInstances) {
            batch.render(instance, environment);
        }
        batch.end();
        Gdx.gl.glViewport(0, 0, 1000, 800);
        renderer.begin(ShapeRenderer.ShapeType.Filled);
        renderer.setColor(Color.WHITE);
        renderer.rect(0, 0, 1000, 300);
        renderer.rect(500, 300, 500, 500);
        renderer.end();
    }

    private void updateInstances() {
        modelInstances.clear();
        for (int i = 0; i < GameManager3D.GRID_X; i++) {
            for (int j = 0; j < GameManager3D.GRID_Y; j++) {
                for (int k = 0; k < GameManager3D.GRID_Z; k++) {
                    if (gameManager.grid[i][j][k]) {
                        ModelInstance instance = new ModelInstance(model, i*10, j*10, k*10);
                        modelInstances.add(instance);
                    }
                }
            }
        }
    }

    @Override
    public void show() {}

    @Override
    public void resize(int width, int height) {}

    @Override
    public void pause() {}

    @Override
    public void resume() {}

    @Override
    public void hide() {}

    @Override
    public void dispose() {
        batch.dispose();
        model.dispose();
    }
}
