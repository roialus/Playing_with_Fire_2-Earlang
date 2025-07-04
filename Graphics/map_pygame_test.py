import pygame
import sys
import math
import random

# Initialize Pygame
pygame.init()

# Enhanced Constants
TILE_SIZE = 48  # Larger tiles for better detail
MAP_SIZE = 16
WINDOW_WIDTH = MAP_SIZE * TILE_SIZE + 300  # Extra space for UI
WINDOW_HEIGHT = MAP_SIZE * TILE_SIZE + 150
FPS = 60

# Professional Color Palette
COLORS = {
    # Floor with gradient effect
    'FLOOR_LIGHT': (245, 235, 205),
    'FLOOR_MID': (230, 220, 190),
    'FLOOR_DARK': (215, 205, 175),
    'FLOOR_SHADOW': (200, 190, 160),

    # Enhanced backgrounds
    'BACKGROUND': (45, 55, 65),
    'UI_BACKGROUND': (35, 45, 55),
    'PANEL_BG': (25, 35, 45),

    # Text with glow
    'TEXT_WHITE': (255, 255, 255),
    'TEXT_GOLD': (255, 215, 0),
    'TEXT_SHADOW': (0, 0, 0),
    'TEXT_CYAN': (100, 255, 255),
    'TEXT_ORANGE': (255, 165, 0),

    # Enhanced brick walls
    'BRICK_TOP': (180, 90, 45),
    'BRICK_MID': (160, 80, 40),
    'BRICK_DARK': (140, 70, 35),
    'BRICK_SHADOW': (120, 60, 30),
    'MORTAR': (100, 50, 25),

    # Beautiful wooden barrels
    'WOOD_LIGHT': (200, 140, 90),
    'WOOD_MID': (180, 120, 70),
    'WOOD_DARK': (160, 100, 50),
    'WOOD_SHADOW': (140, 80, 30),
    'WOOD_HIGHLIGHT': (220, 160, 110),
    'WOOD_BAND': (100, 60, 30),

    # Shiny metal barrels
    'METAL_LIGHT': (160, 165, 170),
    'METAL_MID': (130, 135, 140),
    'METAL_DARK': (100, 105, 110),
    'METAL_SHADOW': (70, 75, 80),
    'METAL_SHINE': (200, 205, 210),
    'METAL_BAND': (60, 65, 70),

    # Vibrant player colors
    'PLAYER_1': (80, 150, 255),  # Bright Blue
    'PLAYER_2': (255, 80, 100),  # Bright Red
    'PLAYER_3': (80, 220, 120),  # Bright Green
    'PLAYER_4': (255, 200, 80),  # Bright Yellow
    'SKIN': (255, 220, 180),
    'SKIN_SHADOW': (230, 195, 155),

    # Glowing power-ups
    'POWERUP_GLOW': (255, 255, 150),
    'POWERUP_CORE': (255, 215, 0),
    'POWERUP_PULSE': (255, 255, 100),

    # Special effects
    'SHADOW': (0, 0, 0, 60),
    'HIGHLIGHT': (255, 255, 255, 100),
    'SELECTION': (255, 255, 0, 150),
    'GRID_LINE': (0, 0, 0, 40),
}


class EnhancedMapVisualizer:
    def __init__(self):
        self.screen = pygame.display.set_mode((WINDOW_WIDTH, WINDOW_HEIGHT))
        pygame.display.set_caption("🎮 Enhanced Bomberman Map - Professional 2D Graphics")
        self.clock = pygame.time.Clock()

        # Enhanced fonts
        self.title_font = pygame.font.Font(None, 36)
        self.font = pygame.font.Font(None, 24)
        self.small_font = pygame.font.Font(None, 18)
        self.powerup_font = pygame.font.Font(None, 20)

        # Animation variables
        self.time = 0
        self.powerup_pulse = 0
        self.camera_shake = 0
        self.selected_tile = None

        # Load map data
        self.map_data = self.load_test_data()

        # Create surface for smooth rendering
        self.map_surface = pygame.Surface((MAP_SIZE * TILE_SIZE, MAP_SIZE * TILE_SIZE))

    def load_test_data(self):
        """Load enhanced map data"""
        tiles = [
            [2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2],
            [2, 4, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 2],
            [2, 0, 0, 0, 1, 1, 0, 1, 0, 3, 1, 0, 3, 0, 0, 2],
            [2, 0, 1, 2, 1, 2, 2, 3, 1, 1, 0, 3, 1, 0, 0, 2],
            [2, 2, 1, 3, 0, 1, 3, 1, 3, 3, 2, 0, 1, 1, 0, 2],
            [2, 2, 0, 1, 1, 1, 1, 1, 1, 0, 1, 3, 0, 0, 0, 2],
            [2, 0, 0, 0, 2, 3, 1, 0, 1, 3, 3, 1, 2, 0, 0, 2],
            [2, 1, 0, 0, 2, 0, 1, 1, 1, 1, 3, 1, 1, 1, 0, 2],
            [2, 0, 3, 0, 0, 1, 1, 1, 1, 2, 3, 0, 0, 0, 0, 2],
            [2, 0, 3, 0, 1, 1, 2, 1, 0, 1, 3, 1, 0, 0, 0, 2],
            [2, 2, 1, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2],
            [2, 0, 1, 2, 2, 0, 1, 1, 1, 2, 0, 3, 0, 0, 1, 2],
            [2, 3, 1, 2, 0, 3, 3, 0, 2, 1, 0, 1, 0, 1, 1, 2],
            [2, 0, 0, 1, 1, 0, 0, 0, 0, 1, 3, 2, 0, 0, 0, 2],
            [2, 4, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 4, 2],
            [2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]
        ]

        powerups = [
            ["none"] * 16,
            ["none", "none", "none", "plus_life", "none", "none", "none", "none", "none", "none", "none", "none",
             "none", "none", "none", "none"],
            ["none", "none", "none", "none", "freeze_bomb", "none", "none", "plus_bombs", "none", "move_speed",
             "plus_life", "none", "phased", "none", "none", "none"],
            ["none", "none", "none", "none", "plus_life", "none", "none", "plus_life", "none", "none", "none",
             "bigger_explosion", "none", "none", "none", "none"],
            ["none", "none", "none", "repeat_bombs", "none", "none", "bigger_explosion", "none", "remote_ignition",
             "bigger_explosion", "none", "none", "bigger_explosion", "none", "none", "none"],
            ["none", "none", "none", "kick_bomb", "remote_ignition", "none", "none", "plus_bombs", "none", "none",
             "none", "remote_ignition", "none", "none", "none", "none"],
            ["none", "none", "none", "none", "none", "bigger_explosion", "none", "none", "plus_life", "move_speed",
             "plus_bombs", "kick_bomb", "none", "none", "none", "none"],
            ["none", "plus_life", "none", "none", "none", "none", "none", "kick_bomb", "plus_bombs", "plus_life",
             "kick_bomb", "move_speed", "remote_ignition", "freeze_bomb", "none", "none"],
            ["none", "none", "plus_life", "none", "none", "repeat_bombs", "none", "plus_bombs", "none", "none",
             "plus_bombs", "none", "none", "none", "none", "none"],
            ["none", "none", "move_speed", "none", "none", "plus_life", "none", "move_speed", "none", "none",
             "kick_bomb", "none", "none", "none", "none", "none"],
            ["none", "none", "remote_ignition", "none", "none", "repeat_bombs", "none", "none", "none", "none", "none",
             "none", "none", "none", "none", "none"],
            ["none", "none", "none", "none", "none", "none", "bigger_explosion", "none", "plus_bombs", "none", "none",
             "bigger_explosion", "none", "none", "plus_life", "none"],
            ["none", "remote_ignition", "remote_ignition", "none", "none", "bigger_explosion", "freeze_bomb", "none",
             "none", "none", "none", "plus_life", "none", "none", "none", "none"],
            ["none", "none", "none", "none", "bigger_explosion", "none", "none", "none", "none", "remote_ignition",
             "plus_life", "none", "none", "none", "none", "none"],
            ["none", "none", "none", "none", "none", "none", "none", "kick_bomb", "none", "phased", "remote_ignition",
             "none", "none", "none", "none", "none"],
            ["none"] * 16
        ]

        return {
            'tiles': tiles,
            'powerups': powerups,
            'player_starts': [
                {'player': 1, 'x': 1, 'y': 1},
                {'player': 2, 'x': 1, 'y': 14},
                {'player': 3, 'x': 14, 'y': 1},
                {'player': 4, 'x': 14, 'y': 14}
            ]
        }

    def draw_gradient_rect(self, surface, color1, color2, rect, vertical=True):
        """Draw a smooth gradient rectangle"""
        if vertical:
            for y in range(rect.height):
                ratio = y / rect.height
                r = int(color1[0] * (1 - ratio) + color2[0] * ratio)
                g = int(color1[1] * (1 - ratio) + color2[1] * ratio)
                b = int(color1[2] * (1 - ratio) + color2[2] * ratio)
                pygame.draw.line(surface, (r, g, b),
                                 (rect.x, rect.y + y), (rect.x + rect.width, rect.y + y))
        else:
            for x in range(rect.width):
                ratio = x / rect.width
                r = int(color1[0] * (1 - ratio) + color2[0] * ratio)
                g = int(color1[1] * (1 - ratio) + color2[1] * ratio)
                b = int(color1[2] * (1 - ratio) + color2[2] * ratio)
                pygame.draw.line(surface, (r, g, b),
                                 (rect.x + x, rect.y), (rect.x + x, rect.y + rect.height))

    def draw_enhanced_shadow(self, surface, x, y, width, height, intensity=60):
        """Draw realistic drop shadow"""
        shadow_surface = pygame.Surface((width + 8, height + 8), pygame.SRCALPHA)
        for i in range(4):
            alpha = intensity - i * 15
            if alpha > 0:
                pygame.draw.rect(shadow_surface, (0, 0, 0, alpha),
                                 (i, i, width, height))
        surface.blit(shadow_surface, (x - 2, y - 2))

    def draw_enhanced_floor(self, surface, x, y):
        """Draw beautiful gradient floor tile"""
        rect = pygame.Rect(x, y, TILE_SIZE, TILE_SIZE)

        # Base gradient
        self.draw_gradient_rect(surface, COLORS['FLOOR_LIGHT'], COLORS['FLOOR_DARK'], rect)

        # Subtle texture lines
        for i in range(4):
            line_y = y + i * (TILE_SIZE // 4)
            pygame.draw.line(surface, COLORS['FLOOR_SHADOW'],
                             (x, line_y), (x + TILE_SIZE, line_y), 1)

        # Border highlight
        pygame.draw.rect(surface, COLORS['FLOOR_LIGHT'], rect, 2)
        pygame.draw.rect(surface, COLORS['FLOOR_SHADOW'], rect, 1)

    def draw_enhanced_brick_wall(self, surface, x, y):
        """Draw professional brick wall with depth"""
        # Drop shadow
        self.draw_enhanced_shadow(surface, x, y, TILE_SIZE, TILE_SIZE, 80)

        # Main wall gradient
        rect = pygame.Rect(x, y, TILE_SIZE, TILE_SIZE)
        self.draw_gradient_rect(surface, COLORS['BRICK_TOP'], COLORS['BRICK_DARK'], rect)

        # Brick pattern with realistic mortar
        brick_height = TILE_SIZE // 4
        for row in range(4):
            brick_y = y + row * brick_height

            # Mortar lines (horizontal)
            pygame.draw.line(surface, COLORS['MORTAR'],
                             (x, brick_y), (x + TILE_SIZE, brick_y), 2)

            # Brick separation (vertical, offset pattern)
            offset = (TILE_SIZE // 3) if row % 2 == 0 else 0
            for i in range(3):
                brick_x = x + offset + i * (TILE_SIZE // 3)
                if x <= brick_x < x + TILE_SIZE:
                    pygame.draw.line(surface, COLORS['MORTAR'],
                                     (brick_x, brick_y), (brick_x, brick_y + brick_height), 2)

        # Depth highlight
        pygame.draw.line(surface, COLORS['BRICK_TOP'], (x, y), (x + TILE_SIZE, y), 2)
        pygame.draw.line(surface, COLORS['BRICK_TOP'], (x, y), (x, y + TILE_SIZE), 2)

    def draw_enhanced_wooden_barrel(self, surface, x, y, has_powerup=False):
        """Draw beautiful wooden barrel with realistic shading"""
        # Drop shadow
        self.draw_enhanced_shadow(surface, x, y, TILE_SIZE, TILE_SIZE, 70)

        # Barrel body with curve effect
        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2

        # Main barrel shape (wider in middle)
        for i in range(TILE_SIZE):
            y_pos = y + i
            # Create barrel curve
            curve_factor = 1.0 + 0.2 * math.sin((i / TILE_SIZE) * math.pi)
            width = int((TILE_SIZE - 12) * curve_factor)

            # Color gradient from top to bottom
            ratio = i / TILE_SIZE
            r = int(COLORS['WOOD_LIGHT'][0] * (1 - ratio) + COLORS['WOOD_DARK'][0] * ratio)
            g = int(COLORS['WOOD_LIGHT'][1] * (1 - ratio) + COLORS['WOOD_DARK'][1] * ratio)
            b = int(COLORS['WOOD_LIGHT'][2] * (1 - ratio) + COLORS['WOOD_DARK'][2] * ratio)

            pygame.draw.line(surface, (r, g, b),
                             (center_x - width // 2, y_pos), (center_x + width // 2, y_pos), 1)

        # Metal bands with depth
        band_positions = [0.2, 0.5, 0.8]
        for band_ratio in band_positions:
            band_y = int(y + TILE_SIZE * band_ratio)
            band_width = int((TILE_SIZE - 8) * (1.0 + 0.2 * math.sin(band_ratio * math.pi)))

            # Band shadow
            pygame.draw.rect(surface, COLORS['WOOD_SHADOW'],
                             (center_x - band_width // 2, band_y - 2, band_width, 5))
            # Band highlight
            pygame.draw.rect(surface, COLORS['WOOD_BAND'],
                             (center_x - band_width // 2, band_y - 3, band_width, 4))
            # Band shine
            pygame.draw.rect(surface, COLORS['WOOD_HIGHLIGHT'],
                             (center_x - band_width // 2, band_y - 3, band_width, 1))

        # Wood grain lines
        for i in range(6):
            grain_x = x + 8 + i * 6
            if grain_x < x + TILE_SIZE - 8:
                pygame.draw.line(surface, COLORS['WOOD_SHADOW'],
                                 (grain_x, y + 6), (grain_x, y + TILE_SIZE - 6), 1)

        # Highlight edge
        pygame.draw.line(surface, COLORS['WOOD_HIGHLIGHT'],
                         (x + 6, y + 4), (x + 6, y + TILE_SIZE - 4), 2)

        # Power-up glow effect
        if has_powerup:
            self.draw_powerup_glow(surface, center_x, center_y)

    def draw_enhanced_metal_barrel(self, surface, x, y, has_powerup=False):
        """Draw shiny metal barrel with reflections"""
        # Drop shadow
        self.draw_enhanced_shadow(surface, x, y, TILE_SIZE, TILE_SIZE, 70)

        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2

        # Main barrel shape with metallic gradient
        for i in range(TILE_SIZE):
            y_pos = y + i
            curve_factor = 1.0 + 0.2 * math.sin((i / TILE_SIZE) * math.pi)
            width = int((TILE_SIZE - 12) * curve_factor)

            # Metallic gradient
            ratio = i / TILE_SIZE
            r = int(COLORS['METAL_LIGHT'][0] * (1 - ratio) + COLORS['METAL_DARK'][0] * ratio)
            g = int(COLORS['METAL_LIGHT'][1] * (1 - ratio) + COLORS['METAL_DARK'][1] * ratio)
            b = int(COLORS['METAL_LIGHT'][2] * (1 - ratio) + COLORS['METAL_DARK'][2] * ratio)

            pygame.draw.line(surface, (r, g, b),
                             (center_x - width // 2, y_pos), (center_x + width // 2, y_pos), 1)

        # Metal bands
        band_positions = [0.25, 0.75]
        for band_ratio in band_positions:
            band_y = int(y + TILE_SIZE * band_ratio)
            band_width = int((TILE_SIZE - 8) * (1.0 + 0.2 * math.sin(band_ratio * math.pi)))

            pygame.draw.rect(surface, COLORS['METAL_BAND'],
                             (center_x - band_width // 2, band_y - 2, band_width, 4))
            pygame.draw.rect(surface, COLORS['METAL_SHINE'],
                             (center_x - band_width // 2, band_y - 2, band_width, 1))

        # Metallic shine/reflection
        shine_width = 4
        pygame.draw.rect(surface, COLORS['METAL_SHINE'],
                         (center_x - 8, y + 6, shine_width, TILE_SIZE - 12))

        # Secondary reflection
        pygame.draw.rect(surface, COLORS['METAL_LIGHT'],
                         (center_x + 4, y + 8, 2, TILE_SIZE - 16))

        # Power-up glow effect
        if has_powerup:
            self.draw_powerup_glow(surface, center_x, center_y)

    def draw_powerup_glow(self, surface, center_x, center_y):
        """Draw beautiful power-up glow effect"""
        glow_intensity = 0.7 + 0.3 * math.sin(self.powerup_pulse * 4)
        glow_size = int(20 + 8 * math.sin(self.powerup_pulse * 3))

        # Multiple glow layers for smooth effect
        for radius in range(glow_size, 0, -3):
            alpha = int(30 * glow_intensity * (radius / glow_size))
            if alpha > 0:
                glow_surf = pygame.Surface((radius * 2, radius * 2), pygame.SRCALPHA)
                pygame.draw.circle(glow_surf, (*COLORS['POWERUP_GLOW'], alpha),
                                   (radius, radius), radius)
                surface.blit(glow_surf, (center_x - radius, center_y - radius))

    def draw_enhanced_player(self, surface, x, y, player_num):
        """Draw beautiful animated player character"""
        center_x = x + TILE_SIZE // 2
        center_y = y + TILE_SIZE // 2

        # Player colors
        player_colors = {
            1: COLORS['PLAYER_1'], 2: COLORS['PLAYER_2'],
            3: COLORS['PLAYER_3'], 4: COLORS['PLAYER_4']
        }
        outfit_color = player_colors.get(player_num, COLORS['PLAYER_1'])

        # Gentle bobbing animation
        bob_offset = math.sin(self.time * 4 + player_num * 1.5) * 2
        char_y = center_y + bob_offset

        # Drop shadow
        shadow_surf = pygame.Surface((TILE_SIZE, 12), pygame.SRCALPHA)
        pygame.draw.ellipse(shadow_surf, COLORS['SHADOW'], (4, 0, TILE_SIZE - 8, 12))
        surface.blit(shadow_surf, (x, y + TILE_SIZE - 8))

        # Body with gradient
        body_rect = pygame.Rect(center_x - 8, char_y - 2, 16, 20)
        self.draw_gradient_rect(surface, outfit_color,
                                tuple(max(0, c - 40) for c in outfit_color), body_rect)
        pygame.draw.rect(surface, tuple(max(0, c - 60) for c in outfit_color), body_rect, 2)

        # Head with skin gradient
        head_y = char_y - 12
        pygame.draw.circle(surface, COLORS['SKIN_SHADOW'], (center_x + 1, head_y + 1), 10)
        pygame.draw.circle(surface, COLORS['SKIN'], (center_x, head_y), 10)
        pygame.draw.circle(surface, tuple(max(0, c - 30) for c in COLORS['SKIN']),
                           (center_x, head_y), 10, 1)

        # Enhanced facial features
        # Eyes with more detail
        # Left eye
        pygame.draw.ellipse(surface, (255, 255, 255), (center_x - 6, head_y - 4, 6, 4))
        pygame.draw.circle(surface, (0, 0, 0), (center_x - 3, head_y - 2), 2)
        pygame.draw.circle(surface, (255, 255, 255), (center_x - 2, head_y - 3), 1)

        # Right eye
        pygame.draw.ellipse(surface, (255, 255, 255), (center_x + 1, head_y - 4, 6, 4))
        pygame.draw.circle(surface, (0, 0, 0), (center_x + 4, head_y - 2), 2)
        pygame.draw.circle(surface, (255, 255, 255), (center_x + 5, head_y - 3), 1)

        # Eyebrows
        pygame.draw.arc(surface, (101, 67, 33), (center_x - 6, head_y - 7, 5, 4), 0, math.pi, 2)
        pygame.draw.arc(surface, (101, 67, 33), (center_x + 2, head_y - 7, 5, 4), 0, math.pi, 2)

        # Nose (small dot)
        pygame.draw.circle(surface, (200, 150, 120), (center_x, head_y), 1)

        # Mouth (happy smile)
        pygame.draw.arc(surface, (150, 100, 80), (center_x - 3, head_y + 1, 6, 4), 0, math.pi, 2)

        # Arms with animation
        arm_swing = math.sin(self.time * 6 + player_num) * 3

        # Left arm
        arm_end_x = center_x - 12 + arm_swing
        arm_end_y = char_y + 6
        pygame.draw.line(surface, outfit_color, (center_x - 6, char_y + 4),
                         (arm_end_x, arm_end_y), 4)
        pygame.draw.circle(surface, COLORS['SKIN'], (arm_end_x, arm_end_y), 3)

        # Right arm
        arm_end_x = center_x + 12 - arm_swing
        pygame.draw.line(surface, outfit_color, (center_x + 6, char_y + 4),
                         (arm_end_x, arm_end_y), 4)
        pygame.draw.circle(surface, COLORS['SKIN'], (arm_end_x, arm_end_y), 3)

        # Legs with walking animation
        leg_offset = math.sin(self.time * 8 + player_num) * 2

        # Left leg
        pygame.draw.rect(surface, outfit_color,
                         (center_x - 6 + leg_offset, char_y + 16, 4, 10))
        pygame.draw.rect(surface, (40, 40, 40),
                         (center_x - 7 + leg_offset, char_y + 24, 6, 4))

        # Right leg
        pygame.draw.rect(surface, outfit_color,
                         (center_x + 2 - leg_offset, char_y + 16, 4, 10))
        pygame.draw.rect(surface, (40, 40, 40),
                         (center_x + 1 - leg_offset, char_y + 24, 6, 4))

        # Player number badge with glow
        badge_surf = pygame.Surface((16, 10), pygame.SRCALPHA)
        pygame.draw.rect(badge_surf, (255, 255, 255, 200), (0, 0, 16, 10))
        pygame.draw.rect(badge_surf, (0, 0, 0), (0, 0, 16, 10), 1)

        num_text = self.small_font.render(str(player_num), True, (0, 0, 0))
        badge_surf.blit(num_text, (5, -1))

        # Badge glow
        glow_surf = pygame.Surface((20, 14), pygame.SRCALPHA)
        pygame.draw.rect(glow_surf, (*COLORS['POWERUP_CORE'], 100), (0, 0, 20, 14))
        surface.blit(glow_surf, (center_x - 10, char_y + 20))
        surface.blit(badge_surf, (center_x - 8, char_y + 22))

    def draw_custom_icon(self, surface, icon_type, center_x, center_y, size, color):
        """Draw custom icons instead of emoji"""
        if icon_type == "lightning":
            # Lightning bolt
            points = [
                (center_x - size // 3, center_y - size // 2),
                (center_x + size // 6, center_y - size // 4),
                (center_x - size // 6, center_y),
                (center_x + size // 3, center_y + size // 2),
                (center_x - size // 6, center_y + size // 4),
                (center_x + size // 6, center_y)
            ]
            pygame.draw.polygon(surface, color, points)

        elif icon_type == "remote":
            # Remote control
            pygame.draw.rect(surface, color, (center_x - size // 3, center_y - size // 2, size // 1.5, size))
            pygame.draw.circle(surface, (255, 100, 100), (center_x - size // 6, center_y - size // 4), 3)
            pygame.draw.circle(surface, (100, 255, 100), (center_x + size // 6, center_y - size // 4), 3)
            pygame.draw.line(surface, color, (center_x, center_y - size // 2), (center_x, center_y - size), 2)

        elif icon_type == "factory":
            # Gear/factory symbol
            pygame.draw.circle(surface, color, (center_x, center_y), size // 2, 3)
            for i in range(8):
                angle = i * math.pi / 4
                x1 = center_x + int((size // 2 - 3) * math.cos(angle))
                y1 = center_y + int((size // 2 - 3) * math.sin(angle))
                x2 = center_x + int((size // 2 + 5) * math.cos(angle))
                y2 = center_y + int((size // 2 + 5) * math.sin(angle))
                pygame.draw.line(surface, color, (x1, y1), (x2, y2), 2)

        elif icon_type == "boot":
            # Leg in side view kicking
            # Thigh
            pygame.draw.line(surface, color, (center_x - size // 3, center_y - size // 4),
                             (center_x, center_y + size // 4), 8)
            # Shin
            pygame.draw.line(surface, color, (center_x, center_y + size // 4),
                             (center_x + size // 2, center_y + size // 6), 6)
            # Foot/boot
            pygame.draw.ellipse(surface, color, (center_x + size // 3, center_y, size // 1.5, size // 3))
            # Motion lines
            for i in range(3):
                line_x = center_x + size // 2 + i * 6
                pygame.draw.line(surface, (*color[:3], 150), (line_x, center_y + size // 8),
                                 (line_x + 8, center_y + size // 8), 2)

        elif icon_type == "ghost":
            # Ghost shape
            pygame.draw.circle(surface, color, (center_x, center_y - size // 4), size // 2)
            pygame.draw.rect(surface, color, (center_x - size // 2, center_y - size // 4, size, size // 2))
            # Wavy bottom
            wave_points = []
            for i in range(5):
                x = center_x - size // 2 + i * (size // 4)
                y = center_y + size // 4 + (5 if i % 2 == 0 else -5)
                wave_points.append((x, y))
            if len(wave_points) > 2:
                pygame.draw.polygon(surface, color, [(center_x - size // 2, center_y + size // 4)] + wave_points + [
                    (center_x + size // 2, center_y + size // 4)])

        elif icon_type == "bomb":
            # Bomb
            pygame.draw.circle(surface, color, (center_x, center_y), size // 2)
            pygame.draw.line(surface, color, (center_x, center_y - size // 2), (center_x - size // 4, center_y - size),
                             3)
            pygame.draw.circle(surface, (255, 200, 0), (center_x - size // 4, center_y - size), 3)

        elif icon_type == "explosion":
            # Explosion burst
            for i in range(8):
                angle = i * math.pi / 4
                x = center_x + int(size // 2 * math.cos(angle))
                y = center_y + int(size // 2 * math.sin(angle))
                pygame.draw.line(surface, color, (center_x, center_y), (x, y), 3)
                pygame.draw.circle(surface, color, (x, y), 3)
            pygame.draw.circle(surface, color, (center_x, center_y), size // 4)

        elif icon_type == "heart":
            # Heart shape
            pygame.draw.circle(surface, color, (center_x - size // 4, center_y - size // 6), size // 3)
            pygame.draw.circle(surface, color, (center_x + size // 4, center_y - size // 6), size // 3)
            points = [
                (center_x - size // 2, center_y),
                (center_x + size // 2, center_y),
                (center_x, center_y + size // 2)
            ]
            pygame.draw.polygon(surface, color, points)

        elif icon_type == "freeze":
            # Snowflake/ice crystal
            pygame.draw.line(surface, color, (center_x, center_y - size // 2), (center_x, center_y + size // 2), 3)
            pygame.draw.line(surface, color, (center_x - size // 2, center_y), (center_x + size // 2, center_y), 3)
            pygame.draw.line(surface, color, (center_x - size // 3, center_y - size // 3),
                             (center_x + size // 3, center_y + size // 3), 3)
            pygame.draw.line(surface, color, (center_x - size // 3, center_y + size // 3),
                             (center_x + size // 3, center_y - size // 3), 3)

    def draw_enhanced_map(self):
        """Draw the complete enhanced map"""
        self.map_surface.fill(COLORS['BACKGROUND'])

        # Update animations
        self.time += 1 / FPS
        self.powerup_pulse += 1 / FPS

        # Draw tiles with enhanced graphics
        for x in range(MAP_SIZE):
            for y in range(MAP_SIZE):
                pixel_x = y * TILE_SIZE
                pixel_y = x * TILE_SIZE

                tile_type = self.map_data['tiles'][x][y]
                powerup = self.map_data['powerups'][x][y]
                has_powerup = powerup != "none"

                # Always draw enhanced floor first
                if tile_type != 2:
                    self.draw_enhanced_floor(self.map_surface, pixel_x, pixel_y)

                # Draw enhanced objects
                if tile_type == 1:  # BREAKABLE
                    self.draw_enhanced_wooden_barrel(self.map_surface, pixel_x, pixel_y, has_powerup)
                elif tile_type == 2:  # UNBREAKABLE
                    self.draw_enhanced_brick_wall(self.map_surface, pixel_x, pixel_y)
                elif tile_type == 3:  # STRONG
                    self.draw_enhanced_metal_barrel(self.map_surface, pixel_x, pixel_y, has_powerup)
                elif tile_type == 4:  # PLAYER_START
                    # Find and draw player
                    for player in self.map_data['player_starts']:
                        if player['x'] == x and player['y'] == y:
                            self.draw_enhanced_player(self.map_surface, pixel_x, pixel_y, player['player'])
                            break

                # Selection highlight
                if self.selected_tile == (x, y):
                    highlight_surf = pygame.Surface((TILE_SIZE, TILE_SIZE), pygame.SRCALPHA)
                    pygame.draw.rect(highlight_surf, COLORS['SELECTION'], (0, 0, TILE_SIZE, TILE_SIZE))
                    self.map_surface.blit(highlight_surf, (pixel_x, pixel_y))

        # Blit map to main screen
        self.screen.blit(self.map_surface, (0, 0))

    def draw_enhanced_ui(self):
        """Draw clean power-ups only UI panel"""
        # UI background with gradient
        ui_rect = pygame.Rect(MAP_SIZE * TILE_SIZE, 0, 300, WINDOW_HEIGHT)
        self.draw_gradient_rect(self.screen, COLORS['UI_BACKGROUND'], COLORS['PANEL_BG'], ui_rect)

        # Title with glow effect
        title_y = 30
        title_text = "POWER-UPS"

        # Title shadow
        title_shadow = self.title_font.render(title_text, True, COLORS['TEXT_SHADOW'])
        self.screen.blit(title_shadow, (MAP_SIZE * TILE_SIZE + 22, title_y + 2))

        # Title main with animated glow
        title_color = COLORS['TEXT_GOLD']
        title_main = self.title_font.render(title_text, True, title_color)
        self.screen.blit(title_main, (MAP_SIZE * TILE_SIZE + 20, title_y))

        # Power-ups with cool names and custom drawn icons
        start_y = 100
        powerup_data = [
            ("lightning", "LIGHTNING SPEED", "move_speed", COLORS['TEXT_CYAN']),
            ("remote", "REMOTE DETONATOR", "remote_ignition", COLORS['TEXT_ORANGE']),
            ("factory", "BOMB FACTORY", "repeat_bombs", COLORS['TEXT_GOLD']),
            ("boot", "POWER KICK", "kick_bomb", (255, 100, 255)),
            ("ghost", "GHOST MODE", "phased", (200, 200, 255)),
            ("bomb", "BOMB ARSENAL", "plus_bombs", (255, 150, 100)),
            ("explosion", "MEGA BLAST", "bigger_explosion", (255, 100, 100)),
            ("heart", "EXTRA LIFE", "plus_life", (255, 100, 150)),
            ("freeze", "FREEZE BOMB", "freeze_bomb", (150, 200, 255))
        ]

        for i, (icon_type, name, powerup_type, color) in enumerate(powerup_data):
            y_pos = start_y + i * 55

            if y_pos < WINDOW_HEIGHT - 60:
                # Animated background for each power-up
                bg_alpha = int(30 + 20 * math.sin(self.time * 2 + i * 0.5))
                bg_surf = pygame.Surface((260, 45), pygame.SRCALPHA)

                # Gradient background
                bg_color1 = (*color[:3], bg_alpha) if len(color) >= 3 else (*color, bg_alpha)
                bg_color2 = (max(0, color[0] - 50), max(0, color[1] - 50), max(0, color[2] - 50), bg_alpha // 2)

                # Create gradient effect manually
                for y in range(45):
                    ratio = y / 45
                    r = int(bg_color1[0] * (1 - ratio) + bg_color2[0] * ratio)
                    g = int(bg_color1[1] * (1 - ratio) + bg_color2[1] * ratio)
                    b = int(bg_color1[2] * (1 - ratio) + bg_color2[2] * ratio)
                    a = int(bg_color1[3] * (1 - ratio) + bg_color2[3] * ratio) if len(bg_color1) > 3 else bg_alpha
                    pygame.draw.line(bg_surf, (r, g, b, a), (0, y), (260, y))

                self.screen.blit(bg_surf, (MAP_SIZE * TILE_SIZE + 15, y_pos - 5))

                # Animated icon with scaling
                icon_scale = 1.0 + 0.1 * math.sin(self.time * 4 + i)
                icon_size = int(20 * icon_scale)

                # Draw custom icon with glow
                icon_surf = pygame.Surface((40, 40), pygame.SRCALPHA)

                # Icon glow effect
                glow_size = int(30 + 8 * math.sin(self.time * 3 + i))
                for r in range(glow_size, 0, -3):
                    glow_alpha = int(40 * (r / glow_size))
                    if glow_alpha > 0:
                        glow_circle = pygame.Surface((r * 2, r * 2), pygame.SRCALPHA)
                        pygame.draw.circle(glow_circle, (*color[:3], glow_alpha), (r, r), r)
                        icon_surf.blit(glow_circle, (20 - r, 20 - r))

                # Draw the custom icon
                self.draw_custom_icon(icon_surf, icon_type, 20, 20, icon_size, color)

                self.screen.blit(icon_surf, (MAP_SIZE * TILE_SIZE + 25, y_pos - 5))

                # Power-up name with enhanced styling
                name_color = color
                name_text = self.powerup_font.render(name, True, name_color)

                # Text shadow
                shadow_text = self.powerup_font.render(name, True, COLORS['TEXT_SHADOW'])
                self.screen.blit(shadow_text, (MAP_SIZE * TILE_SIZE + 72, y_pos + 12))

                # Main text
                self.screen.blit(name_text, (MAP_SIZE * TILE_SIZE + 70, y_pos + 10))

                # Animated underline
                underline_width = int(name_text.get_width() * (0.8 + 0.2 * math.sin(self.time * 5 + i)))
                if underline_width > 0:
                    underline_surf = pygame.Surface((underline_width, 2), pygame.SRCALPHA)
                    underline_alpha = int(100 + 50 * math.sin(self.time * 4 + i))
                    pygame.draw.rect(underline_surf, (*color[:3], underline_alpha), (0, 0, underline_width, 2))
                    self.screen.blit(underline_surf, (MAP_SIZE * TILE_SIZE + 70, y_pos + 30))

    def handle_events(self):
        """Enhanced event handling"""
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                return False
            elif event.type == pygame.KEYDOWN:
                if event.key == pygame.K_ESCAPE:
                    return False
            elif event.type == pygame.MOUSEBUTTONDOWN:
                if event.button == 1:  # Left click
                    mouse_x, mouse_y = pygame.mouse.get_pos()

                    # Check if click is within map area
                    if mouse_x < MAP_SIZE * TILE_SIZE and mouse_y < MAP_SIZE * TILE_SIZE:
                        tile_x = mouse_y // TILE_SIZE
                        tile_y = mouse_x // TILE_SIZE

                        if 0 <= tile_x < MAP_SIZE and 0 <= tile_y < MAP_SIZE:
                            self.selected_tile = (tile_x, tile_y)
                            tile_type = self.map_data['tiles'][tile_x][tile_y]
                            powerup = self.map_data['powerups'][tile_x][tile_y]

                            tile_names = {0: 'FREE', 1: 'WOOD_BARREL', 2: 'BRICK_WALL', 3: 'METAL_BARREL',
                                          4: 'PLAYER_START'}
                            tile_name = tile_names.get(tile_type, 'UNKNOWN')

                            print(f"🎯 Enhanced Click: ({tile_x}, {tile_y}) = {tile_name} + {powerup}")

        return True

    def run(self):
        """Enhanced main game loop"""
        print("🎮 Enhanced Bomberman Map Visualizer Started!")
        print("✨ Professional 2D graphics with smooth animations")
        print("🎨 Gradient shading, realistic shadows, and glow effects")
        print("🖱️ Click tiles to inspect | ESC to exit")

        running = True
        while running:
            running = self.handle_events()

            # Clear screen with gradient background
            bg_rect = pygame.Rect(0, 0, WINDOW_WIDTH, WINDOW_HEIGHT)
            self.draw_gradient_rect(self.screen, COLORS['BACKGROUND'], COLORS['PANEL_BG'], bg_rect)

            # Draw enhanced map and UI
            self.draw_enhanced_map()
            self.draw_enhanced_ui()

            pygame.display.flip()
            self.clock.tick(FPS)

        pygame.quit()
        sys.exit()


if __name__ == "__main__":
    visualizer = EnhancedMapVisualizer()
    visualizer.run()
