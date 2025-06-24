# CS334-Final-Project
## CardBoulevard --- A domain-specific programming language *README adapted from our specification.pdf, located in the docs folder 
CardBoulevard is a programming language intended to make creating card games like poker, blackjack, and games with custom rules and logic, easier for the user and programmer. This current version of CardBoulevard can generate a playable game for users following a variety of constraints. This language takes away the complexities of card and player object management, allowing developers to prototype, test, and run card-based games efficiently. The goal of CardBoulevard is to provide a declarative, human-readable way to manage and define cards and decks, rules, game state changes, and win conditions.

By taking in a user-written file, CardBoulevard interprets the code and generates a playable game that responds to user input as determined by the original code. such as user choices and custom conditional logic. CardBoulevard’s current version can handle a variety of game logic, from simple one-turn games to more complicated games with multiple players, turns, and custom win conditions. It’s structure is designed to be easily readable and accessible, even to users with limited programming experience.

## Design Principles 
CardBoulevard’s design is guided by the goal of creating card games both simple and powerful. The language is made for those who have limited experience programming and who want to experiment with game rules, card dynamics, and win conditions. The language is rooted in simplicity through these key features:

• Simplicity and Readability
The syntax of CardBoulevard is short and sweet. The commands resemble natural language when creating games and helps users understand the logic of the games which they are creating. This simplicity helps users quickly identify and read their own program, allowing for good self-expression and easier modifications of game logic. Since creating custom objects for cards and decks is tedious, especially for those with no programming language, instead we allow users to use a sort of template and create their card in whichever way they see fit. CardBoulevard allows for custom suits– or types– of cards, allowing for infinite possibilities.

• Interactivity and Customization
This language isn’t just for incredibly static programs. The goal of this program is to allow for game-interaction through user-input and game logic. Users can simulate turns, players, and receive feedback based on their game’s logic and what they have customized their game to be. CardBoulevard also allows for customization of cards, letting the possibilities of game-making to be endless.

In general, CardBoulevard is designed with user simplicity and readability in mind, while also functioning as a pow- erful tool to create custom card games without the tedious work of learning a difficult and expansive language. It’s designed to be adaptable and to be used by all, and thinking of basic concepts to let users think of mostly logic and concepts of their desired card game.

## Examples you can run
To run these example programs, type dotnet run filename, where filename is the name of the file while being in the correct repository. For these examples, they are held in the example folder, in the test folder, in the code repository, named example-1.boul, replacing 1 with 2 or 3 for each respective program. So, running dotnet run ../tests/examples/example-1.boul while being in the code repository will start the examples!

## Specification
For a more in-depth specification and formal definition + syntax, please look at the specification pdf. 
