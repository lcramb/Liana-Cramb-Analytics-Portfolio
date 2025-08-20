#CSC401 Summer II Final
#Liana Cramb

import random
from datetime import datetime

#Part 1
class Account:
   
    def __init__(self, balance=0, accountNumber = -1):
        self.balance = float(balance)

        if accountNumber == -1:
            self.accountNumber = random.randint(10000000, 99999999)
        elif accountNumber >= 10000000:
            self.accountNumber = int(accountNumber)
        else:
            raise ValueError("Invalid account number.")
    
        self.transLog = []

    def getAccountNumber(self):
        return self.accountNumber
    
    def getBalance(self):
        return self.balance
    
    def getTransactions(self):
        return self.transLog
    
    def deposit(self, amount):
        amount = float(amount)
        self.balance += amount
        self.addTransaction(f"Deposited {int(amount)}")
    
    def withdraw(self, amount):
        amount = float(amount)
        if amount > self.balance:
            print("Error: Insufficient funds.")
            return
        self.balance -= amount
        self.addTransaction(f"Withdrew {int(amount)}")

    def addTransaction(self, description):
        self.transLog.append(description)
    
#Part 2
class User(Account):

    def __init__(self, name, balance, accountNumber = -1):
        super().__init__(balance, accountNumber)
        self.name = name
        self.createPin()

    def createPin(self):
        self.pin = random.randint(1000, 9999)
        
    def getName(self):
        return self.name
        
    def getPin(self):
        return self.pin

#Part 3

class ATM:
    def __init__(self, filename="accounts.txt"):
        self.accounts = [] 
        try:
            with open(filename) as f:
                next(f) 
                for line in f:
                    if line.strip():
                        acctNumber, name, balance, _ = line.strip().split(",")
                        user = User(name, float(balance), int(acctNumber))
                        self.accounts.append(user)
                        print(f"Created user: {user.getName()}, PIN: {user.getPin()}")
        except Exception as e:
            print("Error reading accounts file:", e)

        self.authorize()

    def authorize(self):
        attempts = 0
        while attempts < 3:
            try:
                pin_input = int(input("Please enter your PIN: "))
            except ValueError:
                print("Invalid input, non-numeric characters not allowed, try again.")
                continue

            user = self.find_user_by_pin(pin_input)
            if user:
                print(f"Welcome {user.getName()}!")
                self.displayMenu(user)
                return
            else:
                attempts += 1
                if attempts < 3:
                    print("Invalid pin, try again.")
                else:
                    print("You have performed too many invalid logins, Goodbye")
                    return

    def find_user_by_pin(self, pin):
        for user in self.accounts:
            if user.getPin() == pin:
                return user
        return None

    def displayMenu(self, user):
        while True:
            print(f"\nHello {user.getName()}, please select an option:")
            print("1. Deposit")
            print("2. Withdraw")
            print("3. Balance")
            print("4. AccountInfo")
            print("5. Exit")

            choice = input("Enter your choice: ")
            try:
                choice = int(choice)
            except ValueError:
                print(f"{choice} invalid choice, non-numeric characters not allowed, try again.")
                continue

            if choice == 1:
                self.deposit(user)
            elif choice == 2:
                self.withdraw(user)
            elif choice == 3:
                self.show_balance(user)
            elif choice == 4:
                self.account_info(user)
            elif choice == 5:
                self.exit(user)
                return
            else:
                print("Enter 1 or 2 or 3 or 4 or 5, try again.")

    def deposit(self, user):
        while True:
            amount = input("Please enter the amount to deposit: ")
            try:
                amount = float(amount)
                if amount < 0:
                    print("Negative amount. Try again.")
                    continue
                user.deposit(amount)
                print(f"Deposit successful. New balance: {user.getBalance()}")
                return
            except ValueError:
                print("Invalid amount. Use digits only.")

    def withdraw(self, user):
        while True:
            amount = input("Please enter the amount to withdraw: ")
            try:
                amount = float(amount)
                if amount < 0:
                    print("Negative amount. Try again.")
                    continue
                if amount > user.getBalance():
                    print("Insufficient funds.")
                    continue
                user.withdraw(amount)
                print(f"Withdrawal successful. New balance: {user.getBalance()}")
                return
            except ValueError:
                print("Invalid amount. Use digits only.")

    def show_balance(self, user):
        user.addTransaction(f"Checked balance: {user.getBalance()}")
        print(f"Your balance is currently {user.getBalance()}")

    def account_info(self, user):
        user.addTransaction("Viewed account info")
        print(f"Your account number is {user.getAccountNumber()}")
        print(f"Your balance is currently {user.getBalance()}")
        print(f"The following people have access to this account: {user.getName()}")

    def exit(self, user):
        confirm = input("Are you sure you want to exit? (Y/N): ").strip().upper()
        if confirm == "N":
            return
        elif confirm == "Y":
            date_time = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
            print(f"\n{user.getName()}, thank you for using the ATM")
            print(f"Your remaining balance is {user.getBalance()}")
            print(f"Your transactions today on {date_time} were:")
            for trans in user.getTransactions():
                print(trans)
        else:
            print("Invalid input. Returning to main menu.")

#Part 4

    def main():
        atm = ATM()

    if __name__ == "__main__":
        main()



