#pragma once

#include <string>
#include <vector>

class monster {

public:	
	std::string monsterName;
	int hp = 15;
	
	int attackP = 10;
	int defenceP = 10;
	int agility = 10;

public:
	virtual void encount();

	virtual void Attack(monster Hero);
	virtual void Defence();
	virtual bool Escape(std::vector<monster> allmonsters);
	virtual void skill();

};

class Slime :
	public monster
{
public:
	std::string monsterName = "Slime";
	int hp = 2;
	int attackP = 2;
	int defenceP = 2;
	int agility = 2;

public:
	void KingSlimeTransformation(std::vector<monster> allmonsters);
	void skill(std::vector<monster> allmonsters);
};

class Kingslime :
	public Slime
{
public:
	std::string monsterName = "Kingslime";
	int hp = 6;

	void skill();
};

