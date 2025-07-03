import pygame
import sys
import os

# Redirect stdout for Erlang communication, and suppress pygame startup output
sys.stdout = open(sys.stdout.fileno(), mode='w', buffering=1, encoding='utf-8', errors='replace')
# sys.stderr = open(os.devnull, 'w')    # Uncomment to suppress stderr output

# Initialize Pygame
pygame.init()
SCREEN_WIDTH, SCREEN_HEIGHT = 800, 600
screen = pygame.display.set_mode((SCREEN_WIDTH, SCREEN_HEIGHT))
pygame.display.set_caption("Playing With Fire 2")   # Set the window title
# Full screen mode option (uncomment to use)
# screen = pygame.display.set_mode((SCREEN_WIDTH, SCREEN_HEIGHT), pygame.FULLSCREEN)

# Colors
WHITE = (255, 255, 255)
BLACK = (0, 0, 0)
GRAY = (160, 160, 160)
RED = (200, 50, 50)
BG_COLOR = (78, 53, 36)  # Warm brown-ish background for consistency

# Fonts
font = pygame.font.SysFont("arial", 48)
title_font = pygame.font.SysFont("impact", 80)

# Global state
current_screen = "main_menu"
dot_count = 1   # Number of dots in loading animation
dot_timer = pygame.time.get_ticks() # Initialize dot timer

# Load images
logo_img = pygame.image.load("assets/logo.png").convert_alpha() # Load logo image
bomb_img = pygame.image.load("assets/bomb.png").convert_alpha() # Load bomb icon image


def scale_image_preserve_ratio(image, max_width=None, max_height=None):
    original_width, original_height = image.get_size()  # Get original dimensions
    aspect_ratio = original_width / original_height # Calculate aspect ratio

    if max_width and not max_height:
        width = max_width   # Set width to max_width
        height = int(width / aspect_ratio)  # Calculate height based on aspect ratio
    elif max_height and not max_width:
        height = max_height # Set height to max_height
        width = int(height * aspect_ratio)  # Calculate width based on aspect ratio
    elif max_width and max_height:
        width = min(max_width, int(max_height * aspect_ratio))  # Set width to the minimum of max_width or calculated width
        height = min(max_height, int(max_width / aspect_ratio)) # Set height to the minimum of max_height or calculated height
    else:
        width, height = original_width, original_height # No scaling, keep original dimensions

    return pygame.transform.smoothscale(image, (width, height)) # Scale the image smoothly


def draw_text(text, y, center=True, size=48, color=BLACK):
    fnt = pygame.font.SysFont(None, size)   # Load the font with specified size
    surface = fnt.render(text, True, color) # Render the text surface
    rect = surface.get_rect(center=(SCREEN_WIDTH // 2, y) if center else (20, y))   # Get the rectangle for the text surface
    screen.blit(surface, rect)  # Draw the text surface on the screen


def draw_screen_template(title, title_y=100):
    screen.fill(BG_COLOR)   # Fill the screen with the background color
    title_surface = title_font.render(title, True, BLACK)   # Render the title surface
    title_rect = title_surface.get_rect(center=(SCREEN_WIDTH // 2, title_y))    # Get the rectangle for the title surface
    screen.blit(title_surface, title_rect)  # Draw the title surface on the screen


BUTTON_COLOR = (240, 120, 60)  # Warm orange for buttons
BUTTON_HOVER = (255, 160, 100)  # Lighter orange for hover effect
TEXT_COLOR = (30, 30, 30)  # Dark text color for contrast


def draw_button(text, y, mouse_pos):
    rect = pygame.Rect(0, 0, 300, 60)   # Create a rectangle for the button
    rect.center = (SCREEN_WIDTH // 2, y)    # Center the rectangle on the screen
    color = BUTTON_HOVER if rect.collidepoint(mouse_pos) else BUTTON_COLOR  # Change color on hover
    pygame.draw.rect(screen, color, rect, border_radius=10)     # Draw the button rectangle
    draw_text(text, y, size=36, color=TEXT_COLOR)   # Draw the button text
    return rect


def draw_play_button_with_icon(y, mouse_pos):
    rect = pygame.Rect(0, 0, 300, 60)
    rect.center = (SCREEN_WIDTH // 2, y)
    color = BUTTON_HOVER if rect.collidepoint(mouse_pos) else BUTTON_COLOR
    pygame.draw.rect(screen, color, rect, border_radius=10)

    # icon + text surfaces
    icon = scale_image_preserve_ratio(bomb_img, max_height=36)  # Scale the bomb icon to fit the button
    fnt = pygame.font.SysFont(None, 36) # Load the font for the text
    text_surface = fnt.render("Play", True, TEXT_COLOR) # Render the text surface

    total_width = icon.get_width() + 10 + text_surface.get_width()  # Calculate total width for icon and text

    icon_x = rect.centerx - total_width // 2 - 15   # Calculate icon x position
    text_x = rect.centerx - 25  # Calculate text x position

    icon_rect = icon.get_rect() # Get the rectangle for the icon
    icon_rect.left = icon_x
    icon_rect.centery = y

    text_rect = text_surface.get_rect() # Get the rectangle for the text
    text_rect.left = text_x
    text_rect.centery = y

    screen.blit(icon, icon_rect)    # Draw the icon on the screen
    screen.blit(text_surface, text_rect)    # Draw the text on the screen

    return rect


def show_main_menu(mouse_pos):
    screen.fill(BG_COLOR)   # Fill the screen with the background color

    logo_scaled = scale_image_preserve_ratio(logo_img, max_width=400)   # Scale the logo image to fit the screen
    logo_rect = logo_scaled.get_rect(center=(SCREEN_WIDTH // 2, 120))   # Get the rectangle for the logo image
    screen.blit(logo_scaled, logo_rect) # Draw the logo image on the screen

    play_btn = draw_play_button_with_icon(300, mouse_pos)   # Draw the play button with icon
    exit_btn = draw_button("Exit", 390, mouse_pos)  # Draw the exit button
    return [("play_clicked", play_btn), ("exit_clicked", exit_btn)]


def show_loading():
    global dot_count, dot_timer # Initialize global variables for loading animation
    draw_screen_template("")

    # Animate dots every 500ms
    if pygame.time.get_ticks() - dot_timer > 500:
        dot_count = (dot_count % 3) + 1
        dot_timer = pygame.time.get_ticks()

    draw_text("Connecting to server" + "." * dot_count, SCREEN_HEIGHT // 2, size=40)


def show_error(mouse_pos):
    draw_screen_template("Failed To Connect")
    retry_btn = draw_button("Retry", 300, mouse_pos)
    return_btn = draw_button("Return To Main Menu", 380, mouse_pos)
    return [("retry_clicked", retry_btn), ("return_to_menu", return_btn)]


def send_event(event_str):
    try:
        sys.stdout.write(event_str + "\n")  # Write the event string to stdout
        sys.stdout.flush()  # Flush the output buffer to ensure the event is sent immediately
    except BrokenPipeError:
        pygame.quit()   # Handle broken pipe error gracefully
        return


def listen_for_commands():
    import threading
    def reader():
        global current_screen
        for line in sys.stdin:  # Read commands from stdin
            cmd = line.strip()
            if cmd in {"show_main_menu", "show_loading", "show_error"}:
                current_screen = cmd

    threading.Thread(target=reader, daemon=True).start()


def main():
    global current_screen
    listen_for_commands()   # Start listening for commands in a separate thread
    current_screen = "show_main_menu"   # Set the initial screen to main menu

    clock = pygame.time.Clock() # Create a clock to control the frame rate
    buttons = []

    running = True
    while running:
        mouse_pos = pygame.mouse.get_pos()  # Get the current mouse position
        screen.fill(BG_COLOR)   # Fill the screen with the background color

        if current_screen == "show_main_menu":
            buttons = show_main_menu(mouse_pos) # Show the main menu
        elif current_screen == "show_loading":
            show_loading()  # Show the loading screen
            buttons = []    # No buttons on the loading screen
        elif current_screen == "show_error":
            buttons = show_error(mouse_pos) # Show the error screen
        else:
            draw_text("Waiting for command...", SCREEN_HEIGHT // 2) # Default case if no valid screen is set
            buttons = []    

        for event in pygame.event.get():    
            if event.type == pygame.QUIT:
                pygame.quit()   # Handle quit event
                running = False # Exit the main loop

            elif event.type == pygame.MOUSEBUTTONDOWN and event.button == 1:    # Check for left mouse button click
                for label, rect in buttons:   # Iterate through the buttons
                    if rect.collidepoint(event.pos):    # Check if the mouse click is within the button rectangle
                        send_event(label)   # Send the event label to stdout
                        if label == "exit_clicked":
                            pygame.quit()   # Handle exit button click
                            running = False 

        pygame.display.flip()   # Update the display with the drawn content
        clock.tick(60)  # Control the frame rate to 60 FPS


if __name__ == "__main__":
    main()
