import pygame
import sys

# Initialize Pygame and set up the screen
pygame.init()   # Initialize Pygame
SCREEN_WIDTH, SCREEN_HEIGHT = 800, 600  # Set screen dimensions
screen = pygame.display.set_mode((SCREEN_WIDTH, SCREEN_HEIGHT)) # Create the screen
pygame.display.set_caption("Playing With Fire 2")   # Set the window title
font = pygame.font.SysFont(None, 48)    # Load a default font

# Load the background image
WHITE = (255, 255, 255) # Define a white color
BLACK = (0, 0, 0)   # Define a black color
GRAY = (160, 160, 160)  # Define a gray color
RED = (200, 50, 50) # Define a red color

# Global state
current_screen = "main_menu"    # Track the current screen
button_clicked = None  # Used to send back to Erlang

def draw_text(text, y, center=True, size=48, color=BLACK):
    font = pygame.font.SysFont(None, size)  # Load the font with the specified size
    text_surface = font.render(text, True, color)   # Render the text surface
    rect = text_surface.get_rect(center=(SCREEN_WIDTH // 2, y)) if center else text_surface.get_rect(topleft=(20, y))   # Get the rectangle for the text surface
    screen.blit(text_surface, rect) # Draw the text surface on the screen

def draw_button(text, y):
    button_rect = pygame.Rect(0, 0, 200, 50)    # Create a rectangle for the button
    button_rect.center = (SCREEN_WIDTH // 2, y) # Center the button rectangle
    pygame.draw.rect(screen, GRAY, button_rect) # Draw the button rectangle
    draw_text(text, y, size=36) # Draw the button text
    return button_rect

def show_main_menu():
    screen.fill(WHITE)  # Fill the screen with white color
    draw_text("Playing With Fire 2", 150, size=60)  # Draw the title text

    play_btn = draw_button("Play", 300) # Draw the play button
    exit_btn = draw_button("Exit", 380) # Draw the exit button
    return [("play_clicked", play_btn), ("exit_clicked", exit_btn)] # Return the button click events

def show_loading():
    screen.fill(WHITE)
    draw_text("Connecting to server...", SCREEN_HEIGHT // 2, size=40)   # Draw the loading text


def show_error():
    screen.fill(WHITE)
    draw_text("Failed To Connect", 150, size=60)    # Draw the error text

    retry_btn = draw_button("Retry", 300)   # Draw the retry button
    return_btn = draw_button("Return To Main Menu", 380)    # Draw the return button
    return [("retry_clicked", retry_btn), ("return_to_menu", return_btn)]


def send_event(event_str):
    sys.stdout.write(event_str + "\n")  # Write the event to stdout
    sys.stdout.flush()  # Flush the output buffer to ensure the event is sent immediately


def listen_for_commands():
    import threading

    def reader():
        global current_screen   # Use global variable to track the current screen
        for line in sys.stdin:
            cmd = line.strip()  # Read a command from stdin
            if cmd in {"show_main_menu", "show_loading", "show_error"}:
                current_screen = cmd    # Update the current screen based on the command

    threading.Thread(target=reader, daemon=True).start()    # Start a thread to listen for commands


# MAIN LOOP
def main():
    global current_screen   # Use global variable to track the current screen
    listen_for_commands()   # Start listening for commands

    clock = pygame.time.Clock() # Create a clock to control the frame rate
    buttons = []    # Initialize buttons list

    while True:
        screen.fill(WHITE)

        # Draw appropriate screen
        if current_screen == "show_main_menu":
            buttons = show_main_menu()  # Show the main menu and get the buttons
        elif current_screen == "show_loading":
            show_loading()  # Show the loading screen
            buttons = []    # No buttons on the loading screen
        elif current_screen == "show_error":
            buttons = show_error()  # Show the error screen and get the buttons
        else:
            draw_text("Waiting for command...", SCREEN_HEIGHT // 2) # Default message if no command received
            buttons = []    # No buttons on the default screen

        # Handle events
        for event in pygame.event.get():    # Process Pygame events
            if event.type == pygame.QUIT:   # Check if the window is closed
                pygame.quit()   # Quit Pygame
                sys.exit()

            elif event.type == pygame.MOUSEBUTTONDOWN and event.button == 1:    # Check for left mouse button click
                for label, rect in buttons:   # Iterate through the buttons
                    if rect.collidepoint(event.pos):    # Check if the mouse click is within the button rectangle
                        send_event(label)   # Send the button click event

        pygame.display.flip()   # Update the display
        clock.tick(60)  # Control the frame rate to 60 FPS


if __name__ == "__main__":
    main()