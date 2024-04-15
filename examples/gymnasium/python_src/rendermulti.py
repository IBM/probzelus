from gymnasium import Wrapper
import gymnasium as gym
import cv2

class RenderMultiHuman(Wrapper) :
    def __init__(self, env, name) :
        super().__init__(env)
        assert (env.render_mode == "rgb_array")
        self.name = name
        cv2.namedWindow(name, flags=cv2.WINDOW_GUI_NORMAL)

    def render(self) :
        rgb_array = self.env.render()
        bgr_array = cv2.cvtColor(rgb_array, cv2.COLOR_BGR2RGB)
        cv2.imshow(self.name,bgr_array)
        cv2.waitKey(1)
        
    def step(self, action) :
        step = self.env.step(action)
        self.render()
        return step

    def reset(self) :
        reset = self.env.reset()
        self.render()
        return reset

    def close(self) :
        self.env.close()
        cv2.destroyWindow(self.name)
