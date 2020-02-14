#include "Slime.h"
#include <iostream>
#include <string>
#include <fstream>
#include <cstdio>
#include <vector>

using namespace std; 

void myturn(monster Hero, std::vector<monster> allmonsters) {
	
	int command, attackMonster;

	std::cout << "Command (1:attack, 2:defence, 3:escape) > " << flush;
	std::cin >> command;


	switch (command) {
	case 1:

		std::cout << "Choose an ID number of a monster to attack on, " << allmonsters.size() << flush;
		std::cin >> attackMonster;

		Hero.Attack(allmonsters[attackMonster]);

		break;

	case 2:
		std::cout << Hero.monsterName << " defended!" << endl;
		Hero.Defence();
		break;

	case 3:
		Hero.Escape(allmonsters);
		break;
	}

}


bool enemyturn(monster Hero, Slime enemy, std::vector<monster> allmonsters) {
	bool ifexit = false;

	int command;
	command = 4;//(rand() % 4) + 1 ;

	// Command (1:attack, 2:defence, 3:escape, 4: call friends)
	switch (command) {
	case 1:
		cout << enemy.monsterName << " attacked to " << Hero.monsterName << "!" << endl;
		enemy.Attack(enemy);
		cout << Hero.monsterName << "'s HP is " << Hero.hp << endl;
		break;

	case 2:
		std::cout << enemy.monsterName << " defended!" << endl;
		enemy.Defence();
		break;
	case 3:
		ifexit = enemy.Escape(allmonsters);
		break;

	case 4:
		enemy.skill(allmonsters);
		break;
	}

	return ifexit;
}

bool Control(monster chara1, std::vector<monster> allmonsters) {
	if ((chara1.hp > 0) && (allmonsters.size() > 0)) {
		return false;
	}
	else {

		cout << "The battle has finished." << endl;
		return true;
	}
}


int main()
{
	bool is_loop = true;
	bool ifexit = false;
	
	monster Hero;
	Slime slime;
	vector<monster> allmonsters;
	Hero.monsterName = "Ore";

	allmonsters.push_back(slime);

	// Append the number of slimes to slime name
	string sname(slime.monsterName);
	sname.append(2, allmonsters.size());	
	slime.monsterName = sname;
	
	slime.encount();

	while (is_loop) {
		
		// Enemy turn
		std::cout << "The turn of enemies." << std::endl;
		for (int i = 0; i < allmonsters.size(); ++i) {
			ifexit = enemyturn(Hero, allmonsters[i], allmonsters);

			if (ifexit == true) {
				break;
			}
		}

		bool res = Control(Hero, allmonsters);
		if (res) break;

		// My turn
		std::cout << endl << "The turn of " << Hero.monsterName << std::endl;
		myturn(Hero, allmonsters);

		res = Control(Hero, allmonsters);
		if (res) break;
	}

	return 0;
}