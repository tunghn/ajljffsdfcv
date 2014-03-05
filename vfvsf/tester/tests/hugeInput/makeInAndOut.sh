num=100001


echo "$num" > output.txt

for i in {1..100000}; do 
echo -n $RANDOM >> output.txt
echo -n " " >> output.txt
done
echo -n $RANDOM >> output.txt

echo "1" > input.txt
cat output.txt >> input.txt
echo -e " \n" >> output.txt
