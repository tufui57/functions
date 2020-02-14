#include "Slime.h"

#include <iostream>
#include <fstream>
#include <string>
#include <cstdlib>
#include <vector>

using namespace std;

void monster::encount() {

	string path;

	if (monsterName == "Slime") {
		path = "C://Users/m_nom/source/repos/C++ training/C++ training/slime/slime.txt";
	}
	else {
		path = "C://Users/m_nom/source/repos/C++ training/C++ training/slime/kingSlime.txt";
	}

	ifstream ifs(path);
	
	char str[256];

	while (ifs.getline(str, 256 - 1)) {
		std::cout << str << std::endl;
	}
	
	cout << monsterName << " has shown up!" << endl; // this will show wrong name, on calling another slime or kingslime
};

void monster::Attack(monster Hero) {
	cout << "attacking" << endl;
	Hero.hp = Hero.hp - (attackP / 2 - Hero.defenceP / 4);
}

void monster::Defence() {
	defenceP = 2 * defenceP;
}

bool monster::Escape(std::vector<monster> allmonsters) {
	
	bool ifexit = false;

	cout << monsterName << " tried to escape!" << endl;
		
	double prob = rand() / 32767.00;

	if (prob >= 0.5) {
		cout << monsterName << " escaped!" << endl;
		ifexit = true;
	}
	else {
		cout << monsterName << " failed in escaping!" << endl;
	}
	return ifexit;
}

void monster::skill() {
	// Hero has no skills;
}

void Slime::skill(std::vector<monster> allmonsters) {

	std::cout << "Slime called her friends!" << std::endl;

	double prob = rand() / 32767.00;

	if (prob >= 0.1) {

		Slime newslime;
		//string sname(newslime.monsterName);
		//sname.append(2, allmonsters.size());
		//newslime.monsterName = sname;

		allmonsters.push_back(newslime);
		
		newslime.encount();

		KingSlimeTransformation(allmonsters);

	}
	else {
		std::cout << "None of her friends came!" << std::endl;
	}

}


void Slime::KingSlimeTransformation(std::vector<monster> allmonsters) {

	if (allmonsters.size() % 2 == 0) {
		
		cout << "Slimes turned into a Kingslime!" << endl;

		allmonsters.pop_back();
		allmonsters.pop_back();

		Kingslime kingslime;
		string sname(kingslime.monsterName);
		sname.append(2, allmonsters.size());

		kingslime.monsterName = sname;

		kingslime.encount();
		
		allmonsters.push_back(kingslime);
	}

}

void Kingslime::skill() {
	std::cout << "kingslime has no skills." << std::endl;
}


//void init_param(monster monster1) {
//
//}