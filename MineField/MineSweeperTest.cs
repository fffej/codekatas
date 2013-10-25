using NUnit.Framework;
using FluentAssertions;

namespace Fatvat.Katas.MineSweeper
{
    [TestFixture]
    public class MineSweeperTest
    {
        // ReSharper disable InconsistentNaming
        [Test]
        public void Given_Simplest_MineField_Can_ProduceOutput()
        {
            const string mineFieldInput = "1 1\n*";
            const string expectedOutput = "*\n";

            Assert.That(MineField.Read(mineFieldInput).Display(), Is.EqualTo(expectedOutput));
        }

        [Test]
        public void Given_Minefield_With_No_Mines_Produce_Empty_Output()
        {
            const string mineFieldInput = "1 1\n-";
            const string expectedOutput = "0\n";

            Assert.That(MineField.Read(mineFieldInput).Display(), Is.EqualTo(expectedOutput));
        }

        // ReSharper restore InconsistentNaming
    }

    public class MineField
    {
        private readonly bool m_HasMine;

        private MineField(bool hasMine)
        {
            m_HasMine = hasMine;
        }

        public static MineField Read(string input)
        {
            var lines = input.Split(new char[] {'\n'});

            bool hasMine = lines[1] == "*";


            return new MineField(hasMine);
        }

        public string Display()
        {
            return !m_HasMine ? "0\n" : "*\n";
        }
    }
}
