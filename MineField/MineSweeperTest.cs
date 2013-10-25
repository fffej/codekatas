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

        // ReSharper restore InconsistentNaming
    }

    public class MineField
    {
        public static MineField Read(string input)
        {
            return new MineField();
        }

        public string Display()
        {
            return "";
        }
    }
}
